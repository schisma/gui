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
import Data.Instrument (Instrument, updateSynthParameterValue)
import Data.Synth (SynthParameter)
import Env (GlobalEnvironment)
import ThirdParty.Nexus as Nexus

type Slots :: forall k. Row k
type Slots = ()

type Input
  = { displayName :: String
    , selectedInstrument :: Instrument
    , size :: Array Int
    , synthParameter :: SynthParameter
    }

type State
  = { displayName :: String
    , radio :: Maybe Nexus.Radio
    , selectedInstrument :: Instrument
    , size :: Array Int
    , synthParameter :: SynthParameter
    }

data Action
  = HandleChange Number
  | Initialize
  | Receive { displayName :: String
            , selectedInstrument :: Instrument
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
      let instrument =
            updateSynthParameterValue state.selectedInstrument synthParameter

      H.modify_ _ { selectedInstrument = instrument
                  , synthParameter = synthParameter
                  }

      H.raise (UpdatedSynthParameter synthParameter instrument)

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
      H.modify_ _ { displayName = record.displayName
                  , selectedInstrument = record.selectedInstrument
                  , size = record.size
                  , synthParameter = record.synthParameter
                  }

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
