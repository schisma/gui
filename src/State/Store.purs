module State.Store where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe(Maybe(..))
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import Env (GlobalEnvironment, GlobalState)

getGlobalState
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => m GlobalState
getGlobalState = do
  { globalState, globalBus } <- asks _.globalEnvironment
  state <- liftEffect $ Ref.read globalState
  pure state

initialGlobalState :: GlobalState
initialGlobalState =
  { ids: { lastInstrumentId: 0 }
  , instruments: []
  , instrumentsFile: ""
  , selectedTrackIndex: Nothing
  , socket: Nothing
  , synths: Nothing
  , trackerFile: ""
  , tracks: []
  }

updateGlobalState
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => GlobalState
  -> m Unit
updateGlobalState state = do
  { globalState, globalBus } <- asks _.globalEnvironment
  liftEffect $ Ref.write state globalState
  liftAff $ Bus.write state globalBus
  pure unit
