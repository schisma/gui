module Components.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array ((!!), catMaybes, elem, head, length, modifyAtIndices, nub)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Record (merge)
import Type.Proxy (Proxy(..))

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Instrument (class ManageInstrument)
import Capabilities.Resources.Midi ( class ManageMidi
                                   , sendMidiChannel
                                   , sendMidiMessage
                                   )
import Capabilities.Resources.Tracker (class ManageTracker)
import Components.HigherOrder.Connect as Connect
import Components.MidiKeyboard as MidiKeyboard
import Components.Settings as Settings
import Components.Synth as Synth
import Components.Tracker as Tracker
import Data.Component (OpaqueSlot)
import Data.Instrument ( Instrument
                       , midiControlChangeMessage
                       , midiControlChangeMessages
                       )
import Data.Midi (allowedCommands, midiMessage)
import Data.Track (Track, toggleMute, toggleSolo, toInstrument)
import Env (GlobalEnvironment)
import State.Global (setTracks)
import ThirdParty.Socket (Socket)

type Slots
  = ( midiKeyboard :: H.Slot MidiKeyboard.Query MidiKeyboard.Output Unit
    , settings :: OpaqueSlot Unit
    , synth :: H.Slot (Const Void) Synth.Output Unit
    , tracker :: H.Slot (Const Void) Tracker.Output Unit
    )

type Input
  = { socket :: Socket
    | Connect.WithGlobalState ()
    }

type State
  = { displayedPanel :: Panel
    , selectedTrackIndices :: Array Int
    , socket :: Socket
    | Connect.WithGlobalState ()
    }

data Action
  = ChangePanel Panel
  | HandleMidiKeyboard MidiKeyboard.Output
  | HandleSynth Synth.Output
  | HandleTracker Tracker.Output
  | Receive { socket :: Socket
            | Connect.WithGlobalState ()
            }
  | SendMidiChannel
  | SendSelectedSynthParametersAsMidiCCMessages

data Panel
  = Configuration
  | SynthControls

derive instance eqPanel :: Eq Panel


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
  { initialState: merge { displayedPanel: Configuration
                        , selectedTrackIndices: []
                        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    ChangePanel panel ->
      H.modify_ _ { displayedPanel = panel }

    HandleMidiKeyboard output ->
      case output of
        MidiKeyboard.ForwardedMessage message ->
          when (elem message.command allowedCommands) do
            state <- H.get

            case selectedInstrument state of
              Nothing -> pure unit
              Just instrument -> when (instrument.midiChannel > 0)
                let formattedMessage =
                      midiMessage
                      message.command
                      message.noteNumber
                      message.value
                      instrument.midiChannel

                in  sendMidiMessage state.socket formattedMessage

    HandleSynth output ->
      case output of
        Synth.UpdatedSynthParameter synthParameter instrument ->
          when (instrument.midiChannel > 0) do
            state <- H.get

            case midiControlChangeMessage instrument synthParameter of
              Nothing -> pure unit
              Just message -> sendMidiMessage state.socket message

    HandleTracker output ->
      case output of
        Tracker.Blurred ->
          H.tell (Proxy :: _ "midiKeyboard") unit MidiKeyboard.Focus

        Tracker.Played ->
          handleAction SendSelectedSynthParametersAsMidiCCMessages

        Tracker.SelectedColumns columns -> do
          H.modify_ _ { selectedTrackIndices = columns }

          handleAction SendMidiChannel
          handleAction SendSelectedSynthParametersAsMidiCCMessages

        Tracker.ToggledMute -> do
          state <- H.get

          let tracks =
                modifyAtIndices
                state.selectedTrackIndices
                toggleMute
                state.globalState.tracks

          setTracks tracks

        Tracker.ToggledSolo -> do
          state <- H.get

          let tracks =
                modifyAtIndices
                state.selectedTrackIndices
                toggleSolo
                state.globalState.tracks

          setTracks tracks

    Receive { globalState, socket } ->
      H.modify_ _ { globalState = globalState
                  , socket = socket
                  }

    SendMidiChannel -> do
      state <- H.get

      case selectedInstrument state of
        Nothing -> pure unit
        Just instrument -> when (instrument.midiChannel > 0) do
          sendMidiChannel state.socket instrument.midiChannel

    SendSelectedSynthParametersAsMidiCCMessages -> do
      state <- H.get

      case selectedInstrument state of
        Nothing -> pure unit
        Just instrument -> when (instrument.midiChannel > 0) do
          let midiMessages = midiControlChangeMessages instrument
          traverse_ (sendMidiMessage state.socket) midiMessages

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
          { }
          HandleMidiKeyboard

      , HH.div
        [ HP.class_ (HH.ClassName "mt-2") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "nav") ]
            [ HH.ul
                [ HP.class_ (HH.ClassName "flex") ]
                [ renderPanelLink state Configuration
                , renderPanelLink state SynthControls
                ]
            ]
        , renderPanel state
        ]
      ]

  renderPanel :: State -> H.ComponentHTML Action Slots m
  renderPanel state =
    let instrument = selectedInstrument state

    in  case state.displayedPanel of
          Configuration ->
            HH.slot
              (Proxy :: _ "settings")
              unit
              Settings.component
              { globalState: state.globalState
              , selectedInstrument: instrument
              }
              absurd

          SynthControls -> renderSynth instrument

  renderPanelLink :: State -> Panel -> H.ComponentHTML Action Slots m
  renderPanelLink state panel =
    let panelLinkClasses =
          if panel == state.displayedPanel then
            [ "nav-link", "active" ]
          else
            [ "nav-link" ]

        linkName = case panel of
          Configuration -> "Configuration"
          SynthControls -> "Synth Controls"

    in  HH.li
          [ HP.classes (map HH.ClassName [ "flex-1", "mr-2" ])
          , HE.onClick \_ -> ChangePanel panel
          ]
          [ HH.a
              [ HP.classes (map HH.ClassName panelLinkClasses)
              , HP.href "#"
              ]
              [ HH.text $ linkName ]
          ]

  renderSynth :: Maybe Instrument -> H.ComponentHTML Action Slots m
  renderSynth maybeInstrument =
    case maybeInstrument of
      Nothing -> HH.div_ []
      Just instrument ->
        HH.slot
          (Proxy :: _ "synth")
          unit
          Synth.component
          { selectedInstrument: instrument }
          HandleSynth

  selectedInstrument :: State -> Maybe Instrument
  selectedInstrument state =
    let tracks = nub $ selectedTracks state
    in  if length tracks /= 1 then
          Nothing
        else
          case head tracks of
            Nothing -> Nothing
            Just track -> toInstrument state.globalState.instruments track

  selectedTracks :: State -> Array Track
  selectedTracks state =
    catMaybes $ map ((!!) state.globalState.tracks) state.selectedTrackIndices
