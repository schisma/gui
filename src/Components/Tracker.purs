module Components.Tracker where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Midi (class ManageMidi)
import Capabilities.Resources.Tracker (class ManageTracker)
import Data.Track (Track, toName)
import Env (GlobalEnvironment)
import ThirdParty.Spreadsheet ( Spreadsheet
                              , exportAsCsv
                              , rowNumbers
                              , spreadsheet
                              , updateData
                              , updateHeaders
                              )

type Slots :: forall k. Row k
type Slots = ()

type Input = { tracks :: Array Track
             }

type State
  = { tracker :: Maybe Spreadsheet
    }

data Action
  = AddColumn Spreadsheet Int
  | Blur
  | Initialize
  | Play Int Int
  | PlayOnlyMidi
  | PlayTracker Spreadsheet
  | Receive { tracks :: Array Track }
  | RemoveColumns Spreadsheet (Array Int)
  | SelectColumns Spreadsheet (Array Int)
  | StopTracker
  | ToggleMuteSelectedTracks Spreadsheet
  | ToggleSoloSelectedTracks Spreadsheet
  | UpdateTrackerData Spreadsheet

data Output
  = AddedColumn Int
  | Blurred
  | Played Int Int
  | RemovedColumns (Array Int)
  | SelectedColumns (Array Int)
  | Stopped
  | ToggledMute
  | ToggledSolo
  | UpdatedTrackerData

data Query a
  = GetTrackerBody (String -> a)
  | UpdateRows (Array (Array String)) a

component
  :: forall m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageMidi m
  => ManageTracker m
  => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: const { tracker: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    AddColumn sheet index -> do
      H.raise (AddedColumn index)

    Blur -> do
      H.raise Blurred

    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter

      let callbacks = { afterChange: \sheet ->
                          HS.notify listener (UpdateTrackerData sheet)
                      , afterCreateCol: \sheet index ->
                          HS.notify listener (AddColumn sheet index)
                      , afterCreateRow: \sheet ->
                          HS.notify listener (UpdateTrackerData sheet)
                      , afterRemoveCol: \sheet columns ->
                          HS.notify listener (RemoveColumns sheet columns)
                      , afterRemoveRow: \sheet ->
                          HS.notify listener (UpdateTrackerData sheet)
                      , onBlur: \sheet ->
                          HS.notify listener Blur
                      , onMute: \sheet ->
                          HS.notify listener (ToggleMuteSelectedTracks sheet)
                      , onPlay: \sheet ->
                          HS.notify listener (PlayTracker sheet)
                      , onPlayOnlyMidi: \sheet ->
                          HS.notify listener PlayOnlyMidi
                      , onSelection: \sheet columns ->
                          HS.notify listener (SelectColumns sheet columns)
                      , onSolo: \sheet ->
                          HS.notify listener (ToggleSoloSelectedTracks sheet)
                      , onStop: \sheet ->
                          HS.notify listener StopTracker
                      }

      tracker <- H.liftEffect $ spreadsheet "tracker" callbacks
      H.modify_ _ { tracker = Just tracker }

    Play start end ->
      H.raise (Played start end)

    PlayOnlyMidi ->
      handleAction (Play (-1) (-1))

    PlayTracker sheet ->
      let numbers = rowNumbers sheet
      in  handleAction (Play numbers.start numbers.end)

    Receive { tracks } -> do
      state <- H.get

      case state.tracker of
        Nothing -> pure unit
        Just tracker -> do
          let headers = map toName tracks

          void $ H.liftEffect $ updateHeaders tracker headers

    RemoveColumns sheet columns -> do
      H.raise (RemovedColumns columns)

    SelectColumns sheet columns -> do
      H.raise (SelectedColumns columns)

    ToggleMuteSelectedTracks sheet -> do
       H.raise ToggledMute

    ToggleSoloSelectedTracks sheet -> do
       H.raise ToggledSolo

    StopTracker ->
      H.raise Stopped

    UpdateTrackerData sheet -> do
      H.raise UpdatedTrackerData

  handleQuery
    :: forall a. Query a
    -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of
    GetTrackerBody reply -> do
      maybeTracker <- H.gets _.tracker
      case maybeTracker of
        Nothing ->
          pure Nothing
        Just tracker ->
          pure (Just (reply $ exportAsCsv tracker))

    UpdateRows rows a -> do
      state <- H.get

      case state.tracker of
        Nothing -> pure unit
        Just tracker ->
          void $ H.liftEffect $ updateData tracker rows

      pure (Just a)

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
     [ HP.class_ (HH.ClassName "my-2") ]
     [ HH.div
         [ HP.class_ (HH.ClassName "tracker") ]
         [ HH.div [ HP.id ("tracker") ] []
         ]
     ]
