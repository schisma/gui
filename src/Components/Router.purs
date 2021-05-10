module Components.Router where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Record (merge)
import Routing.Duplex (parse)
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Navigate (class Navigate, navigate)
import Capabilities.Resources.Instrument (class ManageInstrument)
import Capabilities.Resources.Midi (class ManageMidi)
import Capabilities.Resources.Synth (class ManageSynth, getSynths)
import Capabilities.Resources.Tracker (class ManageTracker)
import Components.ClientError as ClientError
import Components.HigherOrder.Connect as Connect
import Components.Home as Home
import Data.ApplicationError (ApplicationError(..))
import Data.Component (OpaqueSlot)
import Data.Route (Route(..), routeCodec)
import Data.Synth (Synth)
import Env (GlobalEnvironment)
import ThirdParty.Socket (Socket, createSocket)

type State =
  { route :: Maybe Route
  , maybeSocket :: Maybe Socket
  , maybeSynths :: Maybe (NonEmptyArray Synth)
  | Connect.WithGlobalState ()
  }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | Receive { | Connect.WithGlobalState () }

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , clientError :: OpaqueSlot Unit
  )

component
  :: forall m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => LogMessage m
  => ManageInstrument m
  => ManageMidi m
  => ManageSynth m
  => ManageTracker m
  => Navigate m
  => H.Component Query {} Void m
component = Connect.component $ H.mkComponent
  { initialState:
      merge { maybeSocket: Nothing, route: Nothing, maybeSynths: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      socket <- liftEffect $ createSocket 8888
      maybeSynths <- getSynths

      H.modify_ _ { maybeSocket = Just socket
                  , maybeSynths = maybeSynths
                  }

      initialRoute <- hush <<< (parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute

    Receive { globalState } -> do
      H.modify_ _ { globalState = globalState }

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, globalState } <- H.get

      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }

      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, maybeSocket, maybeSynths, globalState } =
    case maybeSocket of
      Nothing ->
        HH.slot
          (Proxy :: _ "clientError")
          unit
          ClientError.component
          { error: NoSocket }
          absurd
      Just socket ->
        case maybeSynths of
          Nothing ->
            HH.slot
              (Proxy :: _ "clientError")
              unit
              ClientError.component
              { error: NoSynths }
              absurd

          Just synths -> case route of
            Nothing -> HH.div_ [ HH.text "Page Not Found" ]

            Just r -> case r of
              Home ->
                HH.slot
                  (Proxy :: _ "home")
                  unit
                  Home.component
                  { socket, synths }
                  absurd

              ClientError error ->
                HH.slot
                  (Proxy :: _ "clientError")
                  unit
                  ClientError.component
                  { error }
                  absurd
