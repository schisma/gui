module Components.Home where

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
import Capabilities.Resources.Tracker (class ManageTracker)
import Components.HigherOrder.Connect as Connect
import Components.MidiKeyboard as MidiKeyboard
import Components.Settings as Settings
import Components.Tracker as Tracker
import Data.Component (OpaqueSlot)
import Env (GlobalEnvironment)

type Slots
  = ( midiKeyboard :: H.Slot MidiKeyboard.Query Void Unit
    , settings :: OpaqueSlot Unit
    , tracker :: H.Slot (Const Void) Tracker.Output Unit
    )

type Input
  = { | Connect.WithGlobalState ()
    }

type State
  = { | Connect.WithGlobalState ()
    }

data Action
  = HandleTracker Tracker.Output
  | Receive { | Connect.WithGlobalState () }

component
  :: forall q m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => LogMessage m
  => ManageInstrument m
  => ManageMidi m
  => ManageTracker m
  => H.Component q Input Void m
component = H.mkComponent
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
    HandleTracker output ->
      case output of
        Tracker.Blurred ->
          H.tell (Proxy :: _ "midiKeyboard") unit MidiKeyboard.Focus

    Receive { globalState } ->
      H.modify_ _ { globalState = globalState }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "mx-2") ]
      [ HH.slot
          (Proxy :: _ "tracker")
          unit
          Tracker.component
          { globalState: state.globalState }
          HandleTracker

      , HH.slot
          (Proxy :: _ "midiKeyboard")
          unit
          MidiKeyboard.component
          { globalState: state.globalState }
          absurd

      , HH.slot
          (Proxy :: _ "settings")
          unit
          Settings.component
          { globalState: state.globalState }
          absurd
      ]
