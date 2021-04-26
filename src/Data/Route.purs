module Data.Route where

import Prelude hiding ((/))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

import Data.ApplicationError (ApplicationError)
import Data.ApplicationError as ApplicationError

data Error
  = NoSynths

data Route
  = ClientError ApplicationError
  | Home

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

clientError :: RouteDuplex' String -> RouteDuplex' ApplicationError
clientError = as ApplicationError.toString
  (ApplicationError.parse >>> note "Malformed error")

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "ClientError": "error" / clientError segment
  , "Home": noArgs
  }
