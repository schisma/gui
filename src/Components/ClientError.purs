module Components.ClientError where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Navigate (class Navigate, navigate)
import Data.ApplicationError (ApplicationError(..))
import Data.Route (Route(..))
import Env (GlobalEnvironment)

type Slots :: forall k. Row k
type Slots = ()

type Input
  = { error :: ApplicationError
    }

type State
  = { error :: ApplicationError
    }

data Action
  = RedirectHome

component
  :: forall q m r
   . MonadAff m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => LogMessage m
  => Navigate m
  => H.Component q Input Void m
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    RedirectHome -> navigate Home

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ HH.p_
        [ HH.text $ renderErrorMessage state.error ]
      , HH.p_
        [ HH.a
            [ HP.href "#"
            , HE.onClick \_ -> RedirectHome
            ]
            [ HH.text "Back Home" ]
        ]
      ]

  renderErrorMessage ::  ApplicationError -> String
  renderErrorMessage error =
    case error of
      NoSocket ->
        """
        No socket connection. Please try reloading the page.
        """
      NoSynths ->
        """
        No synths were found. Check that schisma is installed and accessible
        via the $PATH.
        """
