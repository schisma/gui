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
import Capabilities.Resources.Midi (class ManageMidi)
import Data.Instrument (Instrument)
import Components.Synths.Profit as Profit
import Data.Component (SynthControlOutput)
import Env (GlobalEnvironment)

type Slots
  = ( profit :: H.Slot (Const Void) SynthControlOutput Unit
    )

type Input
  = { selectedInstrument :: Instrument
    }

type State
  = { selectedInstrument :: Instrument
    }

data Action
  = HandleSynth SynthControlOutput
  | Receive { selectedInstrument :: Instrument
            }

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageInstrument m
  => ManageMidi m
  => H.Component q Input SynthControlOutput m
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

  handleAction :: Action -> H.HalogenM State Action Slots SynthControlOutput m Unit
  handleAction = case _ of
    HandleSynth output -> H.raise output

    Receive { selectedInstrument } ->
      H.modify_ _ { selectedInstrument = selectedInstrument }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    let selectedInstrument = state.selectedInstrument

    in  case selectedInstrument.synth.name of
          "Profit" ->
            HH.div
              [ HP.class_ (HH.ClassName "panel") ]
              [ HH.slot
                (Proxy :: _ "profit")
                unit
                Profit.component
                { selectedInstrument }
                HandleSynth
              ]

          _ -> HH.div_ []
