module Capabilities.Resources.Tracker where

import Prelude

import Affjax (Error, Response)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Halogen (HalogenM)

class Monad m <= ManageTracker m where
  getTrackerDataFromFile :: String -> m (Array (Array String))

  updateTrackerData :: String -> String -> m Unit

  play :: String -> String -> Int -> Int -> m (Either Error (Response Json))

  stop :: m Unit

instance manageTrackerHalogenM
  :: ManageTracker m
  => ManageTracker (HalogenM st act slots msg m)
  where

  getTrackerDataFromFile = lift <<< getTrackerDataFromFile

  updateTrackerData file = lift <<< updateTrackerData file

  play trackerFile instrumentsFile startLine =
    lift <<< play trackerFile instrumentsFile startLine

  stop = lift stop
