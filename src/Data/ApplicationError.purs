module Data.ApplicationError where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data ApplicationError
  = NoSocket
  | NoSynths

derive instance eqApplicationError :: Eq ApplicationError
derive instance ordApplicationError :: Ord ApplicationError
derive instance genericRepApplicationError :: Generic ApplicationError _

instance showApplicationError :: Show ApplicationError where
  show = genericShow

parse :: String -> Maybe ApplicationError
parse "NoSocket" = Just NoSocket
parse "NoSynths" = Just NoSynths
parse _ = Nothing

toString :: ApplicationError -> String
toString error = show error
