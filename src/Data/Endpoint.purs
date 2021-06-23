module Data.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

newtype BaseURL = BaseURL String

data Endpoint
  = InstrumentsFromFile { file :: String }
  | ProjectFromFile { file :: String }
  | Play
  | Stop
  | SynthList
  | TrackerFromFile { file :: String }
  | UpdateInstruments
  | UpdateTracker

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "InstrumentsFromFile": "instruments" ? { file: string }
  , "Play": "play" / noArgs
  , "ProjectFromFile": "project" ? { file: string }
  , "Stop": "stop" / noArgs
  , "SynthList": "synth-list" / noArgs
  , "TrackerFromFile": "tracker" ? { file: string }
  , "UpdateInstruments": "instruments" / noArgs
  , "UpdateTracker": "tracker" / noArgs
  }
