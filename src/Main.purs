module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Ref as Ref
import Halogen (Component, hoist, liftEffect, mkTell)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

import AppM (runAppM)
import Config (logEnvironment)
import Components.Router as Router
import Data.Endpoint (BaseURL(..))
import Data.Route (routeCodec)
import Env (Env)
import State.Store (initialGlobalState)

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody

    let baseUrl = BaseURL "http://localhost:3000"

    globalEnvironment <- liftEffect do
      globalState <- Ref.new initialGlobalState
      globalBus <- Bus.make
      pure { globalState, globalBus }

    let
      environment :: Env
      environment = { globalEnvironment, logEnvironment, baseUrl }

      rootComponent :: Component Router.Query {} Void Aff
      rootComponent = hoist (runAppM environment) Router.component

    halogenIO <- runUI rootComponent {} body

    void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
      when (old /= Just new) do
        launchAff_ $ halogenIO.query $ mkTell $ Router.Navigate new
