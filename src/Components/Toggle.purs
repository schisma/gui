module Components.Toggle where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Record (merge)

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Midi (class ManageMidi)
import Data.Component (SynthControlOutput(..))
import Data.Synth (SynthParameter)
import Env (GlobalEnvironment)
import ThirdParty.Nexus as Nexus

type Slots :: forall k. Row k
type Slots = ()

type Input
  = { displayName :: String
    , size :: Int
    , synthParameter :: SynthParameter
    }

type State
  = { displayName :: String
    , size :: Int
    , synthParameter :: SynthParameter
    , toggle :: Maybe Nexus.Toggle
    }

data Action
  = HandleToggle Boolean
  | Initialize
  | Receive { displayName :: String
            , size :: Int
            , synthParameter :: SynthParameter
            }

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageMidi m
  => H.Component q Input SynthControlOutput m
component =
  H.mkComponent
    { initialState: merge { toggle: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots SynthControlOutput m Unit
  handleAction = case _ of
    HandleToggle toggleState -> do
      state <- H.get

      let value = if toggleState then 1.0 else 0.0
      let synthParameter = state.synthParameter { value = value }

      H.modify_ _ { synthParameter = synthParameter }

      H.raise (UpdatedSynthParameter synthParameter)

    Initialize -> do
      state <- H.get
      let synthParameter = state.synthParameter

      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter

      let options = { size: [state.size * 2, state.size]
                    , state: synthParameter.value /= 0.0
                    }
      let callback =
            (\toggleState -> HS.notify listener (HandleToggle toggleState))

      H.getHTMLElementRef (H.RefLabel "toggle") >>= traverse_ \element -> do
        toggle <- H.liftEffect $ Nexus.toggle element options callback
        H.modify_ _ { toggle = Just toggle }

    Receive record -> do
      state <- H.get

      when (state.synthParameter /= record.synthParameter) do
        let maybeToggle = state.toggle

        H.modify_ _ { displayName = record.displayName
                    , size = record.size
                    , synthParameter = record.synthParameter
                    }

        case maybeToggle of
          Nothing -> pure unit
          Just toggle -> do
            let value = record.synthParameter.value /= 0.0
            updatedToggle <- H.liftEffect $ Nexus.updateToggleState toggle value
            H.modify_ _ { toggle = Just updatedToggle }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ HH.div
          [ HP.class_ (HH.ClassName "mx-auto")
          , HP.ref (H.RefLabel "toggle") ]
          [ ]
      , HH.h5
          [ HP.classes (map HH.ClassName [ "text-center", "text-sm" ]) ]
          [ HH.text state.displayName ]
      ]
