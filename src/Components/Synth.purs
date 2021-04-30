module Components.Synth where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Instrument (class ManageInstrument)
import Capabilities.Resources.Midi (class ManageMidi, sendMidiMessage)
import Data.Instrument ( Instrument
                       , midiControlChangeMessage
                       )
import Components.Synths.Profit as Profit
import Data.Synth (SynthParameter)
import Env (GlobalEnvironment)
import ThirdParty.Socket (Socket)

type Slots
  = ( profit :: H.Slot (Const Void) Profit.Output Unit
    )

type Input
  = { selectedInstrument :: Instrument
    , socket :: Maybe Socket
    }

type State
  = { selectedInstrument :: Instrument
    , socket :: Maybe Socket
    }

data Action
  = Receive { selectedInstrument :: Instrument
            , socket :: Maybe Socket
            }
  | HandleProfit Profit.Output
  | SendMidiControlChangeMessage SynthParameter Instrument

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageInstrument m
  => ManageMidi m
  => H.Component q Input Void m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    HandleProfit output ->
      case output of
        Profit.UpdatedSynthParameter synthParameter instrument ->
          handleAction $ SendMidiControlChangeMessage synthParameter instrument

    Receive { selectedInstrument, socket } ->
      H.modify_ _ { selectedInstrument = selectedInstrument
                  , socket = socket
                  }

    SendMidiControlChangeMessage synthParameter instrument -> do
      maybeSocket <- H.gets _.socket

      when (instrument.midiChannel > 0)
        case maybeSocket of
          Nothing -> pure unit
          Just socket -> do
            case midiControlChangeMessage instrument synthParameter of
              Nothing -> pure unit
              Just message -> sendMidiMessage socket message

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "synth-controls") ]
      (renderSelectedSynth state.selectedInstrument)

  renderSelectedSynth :: Instrument -> Array (H.ComponentHTML Action Slots m)
  renderSelectedSynth selectedInstrument =
    case selectedInstrument.synth.name of
      "Profit" ->
        [ HH.slot
            (Proxy :: _ "profit")
            unit
            Profit.component
            { selectedInstrument }
            HandleProfit
        ]
      _ -> []
