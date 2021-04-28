module Components.MidiKeyboard where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array (elem)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.HTML.HTMLElement (focus)

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Midi (class ManageMidi, sendMidiMessage)
import Components.HigherOrder.Connect as Connect
import Data.Midi (allowedCommands, midiMessage)
import Env (GlobalEnvironment)
import State.Global (getSelectedTrackAndInstrument)
import ThirdParty.MidiKeyboard (MidiKeyboardMessage, midiKeyboard)

type Slots :: forall k. Row k
type Slots = ()

type Input
  = { | Connect.WithGlobalState ()
    }

type State
  = { | Connect.WithGlobalState ()
    }

data Action
  = ForwardMessage MidiKeyboardMessage
  | Initialize
  | Receive { | Connect.WithGlobalState () }

data Query a
  = Focus a

component
  :: forall m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageMidi m
  => H.Component Query Input Void m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    ForwardMessage message -> do
      when (elem message.command allowedCommands) do
        globalState <- H.gets _.globalState

        let Tuple _ maybeInstrument = getSelectedTrackAndInstrument globalState

        case maybeInstrument of
          Nothing -> pure unit
          Just instrument -> when (instrument.midiChannel > 0)
            case globalState.socket of
              Nothing -> pure unit
              Just socket ->
                  let formattedMessage =
                        midiMessage
                        message.command
                        message.noteNumber
                        message.value
                        instrument.midiChannel

                  in  sendMidiMessage socket formattedMessage

    Initialize -> do
      state <- H.get

      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter

      let callback = (\message -> HS.notify listener (ForwardMessage message))

      H.getHTMLElementRef (H.RefLabel "midi-keyboard") >>= traverse_ \element ->
        H.liftEffect $ midiKeyboard element callback

    Receive { globalState } -> do
       H.modify_ _ { globalState = globalState }

  handleQuery
    :: forall a. Query a
    -> H.HalogenM State Action Slots Void m (Maybe a)
  handleQuery = case _ of
    Focus a -> do
      H.getHTMLElementRef (H.RefLabel "midi-keyboard") >>= traverse_ \element ->
        H.liftEffect $ focus element

      pure (Just a)

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "pseudo-hidden") ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.ref (H.RefLabel "midi-keyboard")
          ]
      ]
