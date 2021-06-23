module Capabilities.Resources.Instrument where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty (NonEmptyArray)
import Halogen (HalogenM)

import Data.Instrument (Instrument, InstrumentJson)
import Data.Synth (Synth)

class Monad m <= ManageInstrument m where
  getInstrumentsFromFile
    :: NonEmptyArray Synth
    -> String
    -> m (Array Instrument)

  updateInstrumentsFile
    :: String
    -> Array InstrumentJson
    -> m Unit

instance manageInstrumentHalogenM
  :: ManageInstrument m
  => ManageInstrument (HalogenM st act slots msg m)
  where

  getInstrumentsFromFile availableSynths =
    lift <<< getInstrumentsFromFile availableSynths

  updateInstrumentsFile file = lift <<< updateInstrumentsFile file
