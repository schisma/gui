module Config (logEnvironment) where

import Data.String (toLower)
import Env (LogEnvironment(LogDevelopment, LogProduction, LogTest))

foreign import _logEnvironment :: String

logEnvironment :: LogEnvironment
logEnvironment = case toLower _logEnvironment of
  "development" -> LogDevelopment
  "test" -> LogTest
  "production" -> LogProduction
  _ -> LogDevelopment
