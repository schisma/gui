module Capabilities.Resources.Project where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Halogen (HalogenM)

import Data.Project (Project)

class Monad m <= ManageProject m where
  getProjectFromFile
    :: String
    -> m (Maybe Project)

instance manageProjectHalogenM
  :: ManageProject m
  => ManageProject (HalogenM st act slots msg m)
  where

  getProjectFromFile = lift <<< getProjectFromFile
