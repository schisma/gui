module Capabilities.Resources.Instrument where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty (NonEmptyArray)
import Halogen (HalogenM)

import Data.Instrument (Instrument)
import Data.Synth (Synth)

class Monad m <= ManageInstrument m where
  getInstrumentsFromFile
    :: NonEmptyArray Synth
    -> String
    -> m (Array Instrument)

instance manageInstrumentHalogenM
  :: ManageInstrument m
  => ManageInstrument (HalogenM st act slots msg m)
  where

  getInstrumentsFromFile availableSynths =
    lift <<< getInstrumentsFromFile availableSynths
