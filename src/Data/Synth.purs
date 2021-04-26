module Data.Synth where

import Prelude
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)

import Data.Utilities (modifyIfFound)

type SynthParameter
  = { defaultValue :: Number
    , maximum :: Number
    , midiCcNumber :: Int
    , minimum :: Number
    , name :: String
    , step :: Number
    , value :: Number
    }

type Synth
  = { name :: String
    , parameters :: Array SynthParameter
    }

updateSynthParameters :: Synth -> Map String Number -> Synth
updateSynthParameters synth parameters = foldlWithIndex f synth parameters
  where
  f key synth' value =
    let
      params =
        modifyIfFound
          (\p -> p.name == key)
          (\p -> p { value = value })
          synth'.parameters
    in
      synth' { parameters = params }
