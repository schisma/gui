module Components.Tracker where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Array (concatMap, difference, nub, range)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.UUID (toString)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Uncurried (mkEffectFn2)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Midi (class ManageMidi)
import Capabilities.Resources.Tracker (class ManageTracker)
import Data.Instrument (Instrument)
import Data.Track (Track, toName)
import Env (GlobalEnvironment)
import ThirdParty.Spreadsheet ( ContextMenuItem
                              , Spreadsheet
                              , SpreadsheetSelection
                              , exportAsCsv
                              , rowNumbers
                              , spreadsheet
                              , updateContextMenu
                              , updateData
                              , updateHeaders
                              )

type Slots :: forall k. Row k
type Slots = ()

type Input = { instruments :: Array Instrument
             , tracks :: Array Track
             }

type State
  = { tracker :: Maybe Spreadsheet
    }

data Action
  = AddColumn Spreadsheet Int
  | Blur
  | ChangeInstrument String (Array Int)
  | Initialize
  | Play Int Int
  | PlayOnlyMidi
  | PlayTracker Spreadsheet
  | Receive { instruments :: Array Instrument
            , tracks :: Array Track
            }
  | RemoveColumns Spreadsheet (Array Int)
  | SelectColumns Spreadsheet (Array Int)
  | StopTracker
  | ToggleMuteSelectedTracks Spreadsheet
  | ToggleSoloSelectedTracks Spreadsheet
  | UpdateTrackerData Spreadsheet

data Output
  = AddedColumn Int
  | Blurred
  | ChangedInstrument String (Array Int)
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

contextMenuInstrumentCallback
  :: HS.Listener Action
  -> String
  -> Array SpreadsheetSelection
  -> Effect Unit
contextMenuInstrumentCallback listener key selections =
  let uuid = replace (Pattern "instrument:") (Replacement "") key
      toRange selection = range selection.start.col selection.end.col
      trackIndices = difference (nub $ concatMap toRange selections) [0, 1]
  in
      HS.notify listener (ChangeInstrument uuid trackIndices)

contextMenuItems :: HS.Listener Action -> Array Instrument -> Array ContextMenuItem
contextMenuItems listener instruments  =
  let callback = mkEffectFn2 $ contextMenuInstrumentCallback listener
      toItem instrument = { key: "instrument:" <> (toString instrument.id)
                          , name: instrument.name
                          , callback: callback
                          }
      subMenu = { items: map toItem instruments }
  in  [ { key: "instrument"
        , name: "Instrument"
        , submenu: subMenu
        }
      ]

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

    ChangeInstrument uuid trackIndices ->
      H.raise (ChangedInstrument uuid trackIndices)

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

    Receive { instruments, tracks } -> do
      state <- H.get

      case state.tracker of
        Nothing -> pure unit
        Just tracker -> do
          let headers = map (toName instruments) tracks
          { emitter, listener } <- H.liftEffect HS.create
          void $ H.subscribe emitter

          let contextMenu = contextMenuItems listener instruments

          void $ H.liftEffect $ updateHeaders tracker headers
          void $ H.liftEffect $ updateContextMenu tracker contextMenu

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
