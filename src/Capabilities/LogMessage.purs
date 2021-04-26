module Capabilities.LogMessage where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.JSDate (now)
import Data.Log.Level (LogLevel(Debug, Error, Info, Warn))
import Data.Log.Message (Message)
import Data.Log.Tag (empty)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)

class MonadEffect m <= LogMessage m where
  logMessage :: Message -> m Unit

instance logMessagesHalogenM
  :: LogMessage m
  => LogMessage (HalogenM st act slots msg m)
  where

  logMessage = lift <<< logMessage

log :: forall m. LogMessage m => LogLevel -> String -> m Unit
log level message = liftEffect now >>= logMessage <<<
  { level, message, tags: empty, timestamp: _ }

logDebug :: forall m. LogMessage m => String -> m Unit
logDebug = log Debug

logError :: forall m. LogMessage m => String -> m Unit
logError = log Error

logInfo :: forall m. LogMessage m => String -> m Unit
logInfo = log Info

logWarn :: forall m. LogMessage m => String -> m Unit
logWarn = log Warn
