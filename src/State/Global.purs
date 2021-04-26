module State.Global where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array ((!!), (:), filter, length, mapWithIndex, sortWith, updateAt)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)

import Data.Instrument (Instrument)
import Data.Synth (Synth)
import Data.Track (Track, toInstrument)
import Data.Utilities (modifyIfFound)
import Env (GlobalEnvironment, GlobalState)
import State.Store (getGlobalState, updateGlobalState)
import ThirdParty.Socket (Socket)

addInstrument
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Instrument
  -> m Unit
addInstrument instrument = do
  globalState <- getGlobalState

  let id = globalState.ids.lastInstrumentId + 1
  let existingInstruments = globalState.instruments
  let newInstrument = instrument { id = id
                                 , number = length existingInstruments + 1
                                 }

  let state = globalState { instruments = newInstrument : existingInstruments
                          , ids { lastInstrumentId = id }
                          }
  updateGlobalState state

getSelectedTrackAndInstrument
  :: GlobalState
  -> Tuple (Maybe Track) (Maybe Instrument)
getSelectedTrackAndInstrument globalState =
  let trackIndex = fromMaybe (-1) globalState.selectedTrackIndex
      maybeTrack = globalState.tracks !! trackIndex
  in  case maybeTrack of
        Nothing -> Tuple Nothing Nothing
        Just track -> Tuple maybeTrack (toInstrument track globalState.instruments)

removeInstrument
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Instrument
  -> m Unit
removeInstrument instrument = do
  globalState <- getGlobalState
  let instruments = filter ((/=) instrument) globalState.instruments
  let renumberedInstruments = mapWithIndex
        (\index instr -> instr { number = index + 1 })
        $ sortWith (_.number) instruments
  let state = globalState { instruments = renumberedInstruments }
  updateGlobalState state

setInstruments
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Array Instrument
  -> m Unit
setInstruments instruments = do
  globalState <- getGlobalState
  let id = globalState.ids.lastInstrumentId + 1
      newInstruments = mapWithIndex
          (\index instrument -> instrument { id = id + index })
          instruments
  let state = globalState { instruments = newInstruments
                          , ids { lastInstrumentId = id - 1 + length newInstruments }}
  updateGlobalState state

updateInstrument
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Instrument
  -> m Unit
updateInstrument instrument = do
  globalState <- getGlobalState

  let instruments =
        modifyIfFound
        (\i -> i.id == instrument.id)
        (const instrument)
        globalState.instruments
  let state = globalState { instruments = instruments }

  updateGlobalState state

populateSynths
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Maybe (NonEmptyArray Synth)
  -> m Unit
populateSynths synths = do
  globalState <- getGlobalState
  let state = globalState { synths = synths }
  updateGlobalState state

setInstrumentsFile
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => String
  -> m Unit
setInstrumentsFile file = do
  globalState <- getGlobalState
  let state = globalState { instrumentsFile = file }
  updateGlobalState state

setSelectedTrackIndex
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Maybe Int
  -> m Unit
setSelectedTrackIndex selectedTrackIndex = do
  globalState <- getGlobalState
  let state = globalState { selectedTrackIndex = selectedTrackIndex }
  updateGlobalState state

setSocket
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Maybe Socket
  -> m Unit
setSocket socket = do
  globalState <- getGlobalState
  let state = globalState { socket = socket }
  updateGlobalState state

setTrackerFile
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => String
  -> m Unit
setTrackerFile file = do
  globalState <- getGlobalState
  let state = globalState { trackerFile = file }
  updateGlobalState state

setTracks
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Array Track
  -> m Unit
setTracks tracks = do
  globalState <- getGlobalState
  let state = globalState { tracks = tracks }
  updateGlobalState state

updateSelectedTrack
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Track
  -> m Unit
updateSelectedTrack track = do
  globalState <- getGlobalState

  case globalState.selectedTrackIndex of
    Nothing -> pure unit
    Just index ->
      case updateAt index track globalState.tracks of
        Nothing -> pure unit
        Just tracks -> do
          let state = globalState { tracks = tracks }
          updateGlobalState state
