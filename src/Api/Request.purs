module Api.Request where

import Prelude

import Affjax (Request, request)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)

import Data.Endpoint (BaseURL(..), Endpoint, endpointCodec)

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) { endpoint, method } =
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
  , headers: []
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  , timeout: Nothing
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- ask
  response <- liftAff $ request $ defaultRequest baseUrl opts
  pure $ hush $ rmap _.body response
