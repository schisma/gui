module Capabilities.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

import Data.Route (Route)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM
  :: Navigate m
  => Navigate (HalogenM st act slots msg m)
  where

  navigate = lift <<< navigate
