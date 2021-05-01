module Components.Slider where

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
import Data.Instrument ( Instrument
                       , updateSynthParameterValue
                       )
import Data.Synth (SynthParameter)
import Env (GlobalEnvironment)
import State.Global (updateInstrument)
import ThirdParty.Nexus as Nexus

type Slots :: forall k. Row k
type Slots = ()

type Input
  = { displayName :: String
    , selectedInstrument :: Instrument
    , showNumber :: Boolean
    , size :: Array Int
    , synthParameter :: SynthParameter
    }

type State
  = { displayName :: String
    , ignoreOnChangeHandler :: Boolean
    , selectedInstrument :: Instrument
    , showNumber :: Boolean
    , size :: Array Int
    , slider :: Maybe Nexus.Slider
    , synthParameter :: SynthParameter
    }

data Action
  = HandleChange Number
  | Initialize
  | Receive { displayName :: String
            , selectedInstrument :: Instrument
            , showNumber :: Boolean
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
    { initialState: merge { slider: Nothing, ignoreOnChangeHandler: true }
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

      if state.ignoreOnChangeHandler then
        H.modify_ _ { ignoreOnChangeHandler = false }
      else do
        let synthParameter = state.synthParameter { value = value }
        let instrument =
              updateSynthParameterValue state.selectedInstrument synthParameter

        H.modify_ _ { selectedInstrument = instrument
                    , synthParameter = synthParameter
                    }
        updateInstrument instrument

        H.raise (UpdatedSynthParameter synthParameter instrument)

    Initialize -> do
      state <- H.get
      let synthParameter = state.synthParameter

      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter

      let options = { min: synthParameter.minimum
                    , max: synthParameter.maximum
                    , size: state.size
                    , step: synthParameter.step
                    , value: synthParameter.value
                    }
      let callback = (\value -> HS.notify listener (HandleChange value))

      H.getHTMLElementRef (H.RefLabel "slider") >>= traverse_ \sliderElement -> do
        if state.showNumber then
          H.getHTMLElementRef (H.RefLabel "number") >>= traverse_ \number -> do
            slider <- H.liftEffect $
              Nexus.sliderWithNumber sliderElement number options callback
            H.modify_ _ { slider = Just slider }
        else do
            slider <- H.liftEffect $ Nexus.slider sliderElement options callback
            H.modify_ _ { slider = Just slider }

    Receive record -> do
      H.modify_ _ { displayName = record.displayName
                  , selectedInstrument = record.selectedInstrument
                  , showNumber = record.showNumber
                  , size = record.size
                  , synthParameter = record.synthParameter
                  }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    if state.showNumber then
      HH.div_
        [ HH.div
            [ HP.class_ (HH.ClassName "mx-auto")
            , HP.ref (H.RefLabel "slider") ]
            [ ]
        , HH.h5
            [ HP.classes (map HH.ClassName [ "text-center", "text-sm" ]) ]
            [ HH.text state.displayName ]
        , HH.div
            [ HP.class_ (HH.ClassName "mx-auto")
            , HP.ref (H.RefLabel "number") ]
            [ ]
        ]
    else
      HH.div_
        [ HH.div
            [ HP.class_ (HH.ClassName "mx-auto")
            , HP.ref (H.RefLabel "slider") ]
            [ ]
        , HH.h5
            [ HP.classes (map HH.ClassName [ "text-center", "text-sm" ]) ]
            [ HH.text state.displayName ]
        ]
