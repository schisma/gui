module Components.Tracker where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Array (tail)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Record as Record
import Type.Proxy (Proxy(..))

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Midi (class ManageMidi)
import Capabilities.Resources.Tracker ( class ManageTracker
                                      , getTrackerDataFromFile
                                      , play
                                      , stop
                                      , updateTrackerData
                                      )
import Components.HigherOrder.Connect as Connect
import Data.Track ( fromTrackerData
                  , toCsvName
                  , toName
                  )
import Env (GlobalEnvironment)
import State.Global (setTracks)
import ThirdParty.Papaparse (unparse)
import ThirdParty.Spreadsheet ( Spreadsheet
                              , exportAsCsv
                              , rowNumbers
                              , spreadsheet
                              , updateData
                              , updateHeaders
                              )

type Slots :: forall k. Row k
type Slots = ()

type Input
  = { | Connect.WithGlobalState ()
    }

type State
  = { tracker :: Maybe Spreadsheet
    | Connect.WithGlobalState ()
    }

data Action
  = Blur
  | Initialize
  | Play Int Int
  | PlayOnlyMidi Spreadsheet
  | PlayTracker Spreadsheet
  | Receive { | Connect.WithGlobalState () }
  | SelectColumn Spreadsheet (Array Int)
  | StopTracker
  | ToggleMuteSelectedTrack
  | ToggleSoloSelectedTrack
  | UpdateTrackerData Spreadsheet

data Output
  = Blurred
  | Played
  | SelectedColumns (Array Int)
  | ToggledMute
  | ToggledSolo

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageMidi m
  => ManageTracker m
  => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: Record.insert (Proxy :: _ "tracker") Nothing
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Blur -> do
      H.raise Blurred

    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter

      let callbacks = { afterChange: \sheet ->
                          HS.notify listener (UpdateTrackerData sheet)
                      , afterCreateRow: \sheet ->
                          HS.notify listener (UpdateTrackerData sheet)
                      , afterRemoveRow: \sheet ->
                          HS.notify listener (UpdateTrackerData sheet)
                      , onBlur: \sheet ->
                          HS.notify listener Blur
                      , onMute: \sheet ->
                          HS.notify listener ToggleMuteSelectedTrack
                      , onPlay: \sheet ->
                          HS.notify listener (PlayTracker sheet)
                      , onPlayOnlyMidi: \sheet ->
                          HS.notify listener (PlayOnlyMidi sheet)
                      , onSelection: \sheet columns ->
                          HS.notify listener (SelectColumn sheet columns)
                      , onSolo: \sheet ->
                          HS.notify listener ToggleSoloSelectedTrack
                      , onStop: \sheet ->
                          HS.notify listener StopTracker
                      }

      tracker <- H.liftEffect $ spreadsheet "tracker" callbacks
      H.modify_ _ { tracker = Just tracker }

    Play start end -> do
      globalState <- H.gets _.globalState

      result <- play
        globalState.trackerFile
        globalState.instrumentsFile
        start
        end

      case result of
        -- TODO: Some kind of error message
        Left error -> pure unit
        Right response -> H.raise Played

    PlayOnlyMidi sheet -> handleAction (Play (-1) (-1))

    PlayTracker sheet -> do
      let numbers = rowNumbers sheet
      handleAction (Play numbers.start numbers.end)

    Receive { globalState } -> do
      localState <- H.get

      H.modify_ _ { globalState = globalState }

      if localState.globalState.trackerFile == globalState.trackerFile then
        case localState.tracker of
          Nothing -> pure unit
          Just tracker -> do
            let headers = map toName globalState.tracks

            tracker' <- H.liftEffect $ updateHeaders tracker headers
            H.modify_ _ { tracker = Just tracker' }
      else do
        trackerData <- getTrackerDataFromFile globalState.trackerFile
        let tracks = fromTrackerData globalState.instruments trackerData

        setTracks tracks

        case localState.tracker of
          Nothing -> pure unit
          Just tracker -> do
            let headers = map toName tracks
                rows = fromMaybe [] $ tail trackerData

            tracker' <- H.liftEffect $ updateHeaders tracker headers
            tracker'' <- H.liftEffect $ updateData tracker' rows

            H.modify_ _ { tracker = Just tracker'' }

    SelectColumn sheet columns -> do
       H.raise (SelectedColumns columns)

    ToggleMuteSelectedTrack -> do
       H.raise ToggledMute

    ToggleSoloSelectedTrack -> do
       H.raise ToggledSolo

    StopTracker -> stop

    UpdateTrackerData sheet -> do
      globalState <- H.gets _.globalState

      let header = map (toCsvName globalState.instruments) globalState.tracks
          csvHeader = unparse [header]
          csvRows = exportAsCsv sheet
          contents = csvHeader <> "\n" <> csvRows

      updateTrackerData globalState.trackerFile contents

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
     [ HP.class_ (HH.ClassName "my-2") ]
     [ HH.div
         [ HP.class_ (HH.ClassName "tracker") ]
         [ HH.div [ HP.id ("tracker") ] []
         ]
     ]
