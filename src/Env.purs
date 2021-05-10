module Env where

import Prelude
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)

import Data.Endpoint (BaseURL)

type GlobalState
  = { }

type GlobalEnvironment
  = { globalBus :: BusRW GlobalState
    , globalState :: Ref GlobalState
    }

data LogEnvironment
  = LogDevelopment
  | LogTest
  | LogProduction

derive instance eqLogEnvironment :: Eq LogEnvironment
derive instance ordLogEnvironment :: Ord LogEnvironment

type Env
  = { baseUrl :: BaseURL
    , globalEnvironment :: GlobalEnvironment
    , logEnvironment :: LogEnvironment
    }
