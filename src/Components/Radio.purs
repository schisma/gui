module Components.Radio where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe)
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
    , size :: Array Int
    , synthParameter :: SynthParameter
    }

type State
  = { displayName :: String
    , radio :: Maybe Nexus.Radio
    , size :: Array Int
    , synthParameter :: SynthParameter
    }

data Action
  = HandleChange Number
  | Initialize
  | Receive { displayName :: String
            , size :: Array Int
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
    { initialState: merge { radio: Nothing }
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
    HandleChange value -> do
      state <- H.get

      let synthParameter = state.synthParameter { value = value + 1.0 }

      H.modify_ _ { synthParameter = synthParameter }

      H.raise (UpdatedSynthParameter synthParameter)

    Initialize -> do
      state <- H.get
      let synthParameter = state.synthParameter

      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter

      let active = fromMaybe (-1) $ fromNumber (synthParameter.value - 1.0)
      let numberOfButtons = fromMaybe 0 $ fromNumber (synthParameter.maximum - synthParameter.minimum)

      let options = { active
                    , numberOfButtons
                    , size: state.size
                    }
      let callback = (\value -> HS.notify listener (HandleChange value))

      H.getHTMLElementRef (H.RefLabel "radio") >>= traverse_ \element -> do
        radio <- H.liftEffect $ Nexus.radio element options callback
        H.modify_ _ { radio = Just radio }

    Receive record -> do
      state <- H.get

      when (state.synthParameter /= record.synthParameter) do
        let maybeRadio = state.radio

        H.modify_ _ { displayName = record.displayName
                    , size = record.size
                    , synthParameter = record.synthParameter
                    }

        case maybeRadio of
          Nothing -> pure unit
          Just radio -> do
            let value = fromMaybe (-1) $
                        fromNumber (record.synthParameter.value - 1.0)
            updatedRadio <- H.liftEffect $ Nexus.updateRadioIndex radio value
            H.modify_ _ { radio = Just updatedRadio }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ HH.div
          [ HP.classes (map HH.ClassName ["mx-auto", "radio-parent"])
          , HP.ref (H.RefLabel "radio")
          ]
          [ ]
      , HH.h5
          [ HP.classes (map HH.ClassName [ "text-center", "text-sm" ]) ]
          [ HH.text state.displayName ]
      ]
