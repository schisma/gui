module Env where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)

import Data.Endpoint (BaseURL)
import Data.Instrument (Instrument)
import Data.Synth (Synth)
import Data.Track (Track)

type Ids
  = { lastInstrumentId :: Int
    }

type GlobalState
  = { ids :: Ids
    , instruments :: Array Instrument
    , instrumentsFile :: String
    , synths :: Maybe (NonEmptyArray Synth)
    , trackerFile :: String
    , tracks :: Array Track
    }

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
