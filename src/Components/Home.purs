module Components.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array ( (!!)
                  , (:)
                  , catMaybes
                  , elem
                  , head
                  , length
                  , modifyAtIndices
                  , nub
                  , tail
                  )
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Data.UUID (genUUID)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Record (merge)
import Type.Proxy (Proxy(..))

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Instrument ( class ManageInstrument
                                         , createInstrument
                                         , getInstrumentsFromFile
                                         )
import Capabilities.Resources.Midi ( class ManageMidi
                                   , sendMidiChannel
                                   , sendMidiMessage
                                   )
import Capabilities.Resources.Tracker ( class ManageTracker
                                      , getTrackerDataFromFile
                                      , play
                                      , stop
                                      , updateTrackerData
                                      )
import Components.MidiKeyboard as MidiKeyboard
import Components.Settings as Settings
import Components.Synth as Synth
import Components.Tracker as Tracker
import Data.Component (SynthControlOutput(..))
import Data.Instrument ( Instrument
                       , midiControlChangeMessage
                       , midiControlChangeMessages
                       , remove
                       )
import Data.Midi (allowedCommands, midiMessage)
import Data.Synth (Synth)
import Data.Track ( Track
                  , fromTrackerData
                  , toggleMute
                  , toggleSolo
                  , toCsvName
                  , toInstrument
                  )
import Data.Utilities (modifyIfFound)
import Env (GlobalEnvironment)
import ThirdParty.Papaparse (unparse)
import ThirdParty.Socket (Socket)

type Slots
  = ( midiKeyboard :: H.Slot MidiKeyboard.Query MidiKeyboard.Output Unit
    , settings :: H.Slot (Const Void) Settings.Output Unit
    , synth :: H.Slot (Const Void) SynthControlOutput Unit
    , tracker :: H.Slot Tracker.Query Tracker.Output Unit
    )

type Input
  = { socket :: Socket
    , synths :: NonEmptyArray Synth
    }

type State
  = { displayedPanel :: Panel
    , instruments :: Array Instrument
    , instrumentsFile :: String
    , selectedTrackIndices :: Array Int
    , socket :: Socket
    , synths :: NonEmptyArray Synth
    , trackerFile :: String
    , tracks :: Array Track
    }

data Action
  = ChangePanel Panel
  | HandleMidiKeyboard MidiKeyboard.Output
  | HandleSettings Settings.Output
  | HandleSynth SynthControlOutput
  | HandleTracker Tracker.Output
  | Receive { socket :: Socket
            , synths :: NonEmptyArray Synth
            }
  | SendMidiChannel
  | SendSelectedSynthParametersAsMidiCCMessages
  | UpdateInstrument Instrument
  | UpdateTrackerData String

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
                        , instruments: []
                        , instrumentsFile: "~/code/compositions/proof/instruments.json"
                        , selectedTrackIndices: []
                        , trackerFile: "~/code/compositions/proof/tracker2.csv"
                        , tracks: []
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

    HandleSettings output ->
      case output of
        Settings.AddedInstrument -> do
          state <- H.get

          let number = length state.instruments + 1
          instrument <- createInstrument state.synths number

          H.modify_ _ { instruments = instrument : state.instruments }

        Settings.ChangedInstrumentsFile file -> do
          synths <- H.gets _.synths

          instruments <- getInstrumentsFromFile synths file

          H.modify_ _ { instruments = instruments
                      , instrumentsFile = file
                      }

        Settings.ChangedTrackerFile file -> do
          state <- H.get

          trackerData <- getTrackerDataFromFile file
          let tracks = fromTrackerData state.instruments trackerData
              rows = fromMaybe [] $ tail trackerData

          H.modify_ _ { trackerFile = file
                      , tracks = tracks
                      }
          H.tell (Proxy :: _ "tracker") unit (Tracker.UpdateRows rows)

        Settings.ClonedInstrument instrument -> do
          state <- H.get
          uuid <- H.liftEffect genUUID

          let number = length state.instruments + 1
          let clonedInstrument = instrument { id = uuid, number = number }

          H.modify_ _ { instruments = clonedInstrument : state.instruments }

        Settings.RemovedInstrument instrument -> do
          state <- H.get
          H.modify_ _ { instruments = remove state.instruments instrument }

        Settings.UpdatedInstrument instrument ->
          handleAction (UpdateInstrument instrument)

    HandleSynth output ->
      case output of
        UpdatedSynthParameter synthParameter instrument -> do
          handleAction (UpdateInstrument instrument)

          when (instrument.midiChannel > 0) do
            state <- H.get

            case midiControlChangeMessage instrument synthParameter of
              Nothing -> pure unit
              Just message -> sendMidiMessage state.socket message

    HandleTracker output ->
      case output of
        Tracker.Blurred ->
          H.tell (Proxy :: _ "midiKeyboard") unit MidiKeyboard.Focus

        Tracker.Played start end -> do
          state <- H.get
          result <- play state.trackerFile state.instrumentsFile start end

          case result of
            -- TODO: Some kind of error message
            Left error -> pure unit
            Right response ->
              handleAction SendSelectedSynthParametersAsMidiCCMessages

        Tracker.SelectedColumns columns -> do
          H.modify_ _ { selectedTrackIndices = columns }

          handleAction SendMidiChannel
          handleAction SendSelectedSynthParametersAsMidiCCMessages

        Tracker.Stopped ->
          stop

        Tracker.ToggledMute trackerBody -> do
          state <- H.get

          let tracks =
                modifyAtIndices
                state.selectedTrackIndices
                toggleMute
                state.tracks

          H.modify_ _ { tracks = tracks }
          handleAction (UpdateTrackerData trackerBody)

        Tracker.ToggledSolo trackerBody -> do
          state <- H.get

          let tracks =
                modifyAtIndices
                state.selectedTrackIndices
                toggleSolo
                state.tracks

          H.modify_ _ { tracks = tracks }
          handleAction (UpdateTrackerData trackerBody)

        Tracker.UpdatedTrackerData trackerBody ->
          handleAction (UpdateTrackerData trackerBody)

    Receive { socket, synths } ->
      H.modify_ _ { socket = socket
                  , synths = synths
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

    UpdateInstrument instrument -> do
      state <- H.get

      let instruments =
            modifyIfFound
            (\i -> i.id == instrument.id)
            (const instrument)
            state.instruments

      H.modify_ _ { instruments = instruments }

    UpdateTrackerData trackerBody -> do
      state <- H.get

      let header = map (toCsvName state.instruments) state.tracks
          trackerHeader = unparse [header]
          contents = trackerHeader <> "\n" <> trackerBody

      updateTrackerData state.trackerFile contents

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "mx-2") ]
      [ HH.slot
          (Proxy :: _ "tracker")
          unit
          Tracker.component
          { tracks: state.tracks }
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
              { instruments: state.instruments
              , instrumentsFile: state.instrumentsFile
              , selectedInstrument: instrument
              , synths: state.synths
              , trackerFile: state.trackerFile
              }
              HandleSettings

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
            Just track -> toInstrument state.instruments track

  selectedTracks :: State -> Array Track
  selectedTracks state =
    catMaybes $ map ((!!) state.tracks) state.selectedTrackIndices
