module Api.FormRequest where

import Prelude

import Affjax (Error, Request, Response, request)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.FormURLEncoded (fromArray)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)

import Data.Endpoint (BaseURL(..), Endpoint, endpointCodec)

data FormRequestMethod
  = Put (Array (Tuple String (Maybe String)))
  | Post (Array (Tuple String (Maybe String)))

type FormRequestOptions =
  { endpoint :: Endpoint
  , method :: FormRequestMethod
  }

defaultFormRequest :: BaseURL -> FormRequestOptions -> Request Json
defaultFormRequest (BaseURL baseUrl) { endpoint, method } =
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
  , headers: []
  , content: Just (RB.formURLEncoded $ fromArray body)
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  , timeout: Nothing
  }
  where
  Tuple method body = case method of
    Post b -> Tuple POST b
    Put b -> Tuple PUT b

mkFormRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => FormRequestOptions
  -> m (Either Error (Response Json))
mkFormRequest opts = do
  { baseUrl } <- ask
  let formRequest = (defaultFormRequest baseUrl opts)
                    { headers = [ ContentType applicationFormURLEncoded ] }
  response <- liftAff $ request formRequest
  pure response
