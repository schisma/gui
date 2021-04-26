module Components.HigherOrder.Connect where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (forkAff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

import Env (GlobalEnvironment, GlobalState)
import State.Store (initialGlobalState)

data Action input output
  = Emit output
  | HandleGlobalBus GlobalState
  | Initialize
  | Receive input

type WithGlobalState r
  = ( globalState :: GlobalState | r
    )

type ChildSlots query output
  = ( inner :: H.Slot query output Unit
    )

busEventSource
  :: forall m r i o
   . MonadAff m
  => Bus.BusR' r GlobalState
  -> m (HS.Emitter (Action i o))
busEventSource bus = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ forkAff $ forever do
    val <- Bus.read bus
    H.liftEffect $ HS.notify listener (HandleGlobalBus val)
  pure emitter

component
  :: forall query input output m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => Row.Lacks "globalState" input
  => H.Component query { | WithGlobalState input } output m
  -> H.Component query { | input } output m
component innerComponent =
  H.mkComponent
  { initialState: Record.insert (Proxy :: _ "globalState") initialGlobalState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where

  handleAction = case _ of
    Emit output ->
      H.raise output

    HandleGlobalBus appState ->
      H.modify_ _ { globalState = appState }

    Initialize -> do
      { globalState, globalBus } <- asks _.globalEnvironment

      void $ H.subscribe =<< busEventSource globalBus

      appState <- H.liftEffect $ Ref.read globalState
      H.modify_ _ { globalState = appState }

    Receive input -> do
      { globalState } <- H.get
      H.put $ Record.insert (Proxy :: _ "globalState") globalState input

  handleQuery :: forall a. query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = H.query (Proxy :: _ "inner") unit

  render state = HH.slot (Proxy :: _ "inner") unit innerComponent state Emit
