module Components.MidiKeyboard where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.HTML.HTMLElement (focus)

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Midi (class ManageMidi)
import Env (GlobalEnvironment)
import ThirdParty.MidiKeyboard (MidiKeyboardMessage, midiKeyboard)

type Slots :: forall k. Row k
type Slots = ()

type Input = {}

type State = {}

data Action
  = ForwardMessage MidiKeyboardMessage
  | Initialize

data Output
  = ForwardedMessage MidiKeyboardMessage

data Query a
  = Focus a

component
  :: forall m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageMidi m
  => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    ForwardMessage message -> H.raise (ForwardedMessage message)

    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter

      let callback = (\message -> HS.notify listener (ForwardMessage message))

      H.getHTMLElementRef (H.RefLabel "midi-keyboard") >>= traverse_ \element ->
        H.liftEffect $ midiKeyboard element callback

  handleQuery
    :: forall a. Query a
    -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of
    Focus a -> do
      H.getHTMLElementRef (H.RefLabel "midi-keyboard") >>= traverse_ \element ->
        H.liftEffect $ focus element

      pure (Just a)

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "pseudo-hidden") ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.ref (H.RefLabel "midi-keyboard")
          ]
      ]
