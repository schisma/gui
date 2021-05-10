module AppM where

import Prelude

import Control.Monad.Reader.Trans
  ( class MonadAsk
  , ReaderT
  , ask
  , asks
  , runReaderT
  )
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Data.Log.Level (LogLevel(Debug, Error, Info, Warn))
import Data.Maybe (Maybe(..))
import Data.JSDate (toISOString)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID)
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

import Api.FormRequest (mkFormRequest)
import Api.FormRequest as FormRequest
import Api.Request (mkRequest)
import Api.Request as Request
import Capabilities.LogMessage (class LogMessage)
import Capabilities.Navigate (class Navigate)
import Capabilities.Resources.Instrument (class ManageInstrument)
import Capabilities.Resources.Midi (class ManageMidi)
import Capabilities.Resources.Synth (class ManageSynth)
import Capabilities.Resources.Tracker (class ManageTracker)
import Data.Endpoint (Endpoint(..))
import Data.Instrument (fromInstrumentJson)
import Data.Route (routeCodec)
import Env (Env, LogEnvironment(LogDevelopment))
import ThirdParty.Socket as Socket

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

newtype ParAppM a
  = ParAppM (ReaderT Env ParAff a)

derive newtype instance functorParAppM :: Functor ParAppM
derive newtype instance applyParAppM :: Apply ParAppM
derive newtype instance applicativeParAppM :: Applicative ParAppM

instance parallelAppM :: Parallel ParAppM AppM where
  parallel (AppM readerT) = ParAppM (parallel readerT)
  sequential (ParAppM readerT) = AppM (sequential readerT)


instance logMessageAppM :: LogMessage AppM where
  logMessage message = do
    isoTimestamp <- liftEffect $ toISOString message.timestamp
    let
      formattedMessage = "[" <> isoTimestamp <> "] " <> message.message
    env <- ask
    liftEffect case env.logEnvironment, message.level of
      LogDevelopment, Debug -> Console.log formattedMessage
      LogDevelopment, Error -> Console.error formattedMessage
      LogDevelopment, Info -> Console.info formattedMessage
      LogDevelopment, Warn -> Console.warn formattedMessage
      _, _ -> pure unit


instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print routeCodec


instance manageInstrumentAppM :: ManageInstrument AppM where
  createInstrument availableSynths number = do
    uuid <- liftEffect genUUID
    let instrument = { availableSynths
                     , id: uuid
                     , midiChannel: 0
                     , name: "New Instrument"
                     , number
                     , soundFontPath: ""
                     , synth: head availableSynths
                     }
    pure instrument

  getInstrumentsFromFile availableSynths file = do
    result <- mkRequest { endpoint: InstrumentsFromFile { file }
                        , method: Request.Get
                        }
    case result of
      Nothing -> pure []
      Just response -> case decodeJson response of
        Left decodeError -> pure []
        Right decoded ->
          for decoded \json -> do
            uuid <- liftEffect genUUID
            pure $ fromInstrumentJson availableSynths uuid json


instance manageMidiAppM :: ManageMidi AppM where
  sendMidiChannel socket channel =
    liftEffect $ Socket.sendMidiChannel socket channel

  sendMidiMessage socket message =
    liftEffect $ Socket.sendMidiMessage socket message


instance manageSynthAppM :: ManageSynth AppM where
  getSynths = do
    result <- mkRequest { endpoint: SynthList, method: Request.Get }
    let
      synths = case result of
        Nothing -> Nothing
        Just response -> case decodeJson response of
          Left decodeError -> Nothing
          Right decoded -> Just decoded

    pure synths


instance manageTrackerAppM :: ManageTracker AppM where
  getTrackerDataFromFile file = do
    result <- mkRequest { endpoint: TrackerFromFile { file }
                        , method: Request.Get
                        }
    case result of
      Nothing -> pure []
      Just response -> case decodeJson response of
        Left decodeError -> pure []
        Right decoded -> pure decoded

  updateTrackerData file contents = do
     -- TODO: Check status of request and show error message
    void $ mkFormRequest { endpoint: UpdateTracker
                         , method: FormRequest.Put
                           [ Tuple "contents" (Just contents)
                           , Tuple "file" (Just file)
                           ]
                         }
    pure unit

  play trackerFile instrumentsFile startLine endLine =
    mkFormRequest { endpoint: Play
                  , method: FormRequest.Put
                    [ Tuple "endLine" (Just $ show endLine)
                    , Tuple "instrumentsFile" (Just instrumentsFile)
                    , Tuple "startLine" (Just $ show startLine)
                    , Tuple "trackerFile" (Just trackerFile)
                    ]
                  }

  stop = void $ mkRequest { endpoint: Stop
                          , method: Request.Put (Just $ encodeJson { })
                          }
