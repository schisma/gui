module Data.Utilities where

import Prelude

import Math ((%), ceil, floor, round)

import Data.Array (filter, findIndex, length, modifyAt, notElem, range, zip)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)

deleteAtIndices :: forall a. Array Int -> Array a -> Array a
deleteAtIndices indices xs =
  let withIndices = zip (range 0 (length xs - 1)) xs
      filtered = filter (\(Tuple index x) -> notElem index indices) withIndices
  in  map snd filtered

modifyIfFound :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Array a
modifyIfFound f g xs =
  case findIndex f xs of
    Nothing -> xs
    Just index -> case modifyAt index g xs of
      Nothing -> xs
      Just modified -> modified

roundHalfAwayFromZero :: Number -> Number
roundHalfAwayFromZero number =
  let fraction = number % 1.0
  in  if fraction == 0.5 then
        if number < 0.0 then
          floor number
        else
          ceil number
      else
        round number

roundToNearestMultiple :: Number -> Number -> Number
roundToNearestMultiple number multiple =
  if multiple == 0.0 then
    number
  else
    (roundHalfAwayFromZero $ number / multiple) * multiple

scale :: Number -> Number -> Number -> Number -> Number -> Number
scale value lowerBound upperBound scaledMin scaledMax =
  (value - lowerBound)
    * (scaledMax - scaledMin)
    / (upperBound - lowerBound)
    + scaledMin
