module State.Global where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array ((:), filter, length, mapWithIndex, sortWith)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)

import Data.Instrument (Instrument)
import Data.Synth (Synth)
import Data.Track (Track)
import Data.Utilities (modifyIfFound)
import Env (GlobalEnvironment)
import State.Store (getGlobalState, updateGlobalState)

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
