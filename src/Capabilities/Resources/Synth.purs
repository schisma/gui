module Capabilities.Resources.Synth where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Halogen (HalogenM)

import Data.Synth (Synth)

class Monad m <= ManageSynth m where
  getSynths :: m (Maybe (NonEmptyArray Synth))

  randomizeParameters :: Synth -> m Synth

instance manageSynthHalogenM
  :: ManageSynth m
  => ManageSynth (HalogenM st act slots msg m)
  where

  getSynths = lift getSynths

  randomizeParameters = lift <<< randomizeParameters
