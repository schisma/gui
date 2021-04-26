module Data.Utilities where

import Prelude

import Data.Array (findIndex, modifyAt)
import Data.Maybe (Maybe(..))

modifyIfFound :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Array a
modifyIfFound f g xs =
  case findIndex f xs of
    Nothing -> xs
    Just index -> case modifyAt index g xs of
      Nothing -> xs
      Just modified -> modified

scale :: Number -> Number -> Number -> Number -> Number -> Number
scale value lowerBound upperBound scaledMin scaledMax =
  (value - lowerBound)
    * (scaledMax - scaledMin)
    / (upperBound - lowerBound)
    + scaledMin
