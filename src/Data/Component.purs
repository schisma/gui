module Data.Component where

import Prelude

import Halogen (Slot)

import Data.Instrument (Instrument)
import Data.Synth (SynthParameter)

type OpaqueSlot slot = forall query. Slot query Void slot

data SynthControlOutput
  = RandomizedSynthParameters Instrument
  | UpdatedSynthParameter SynthParameter
