module ThirdParty.Nexus where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import Web.HTML (HTMLElement)

foreign import data Dial ∷ Type

foreign import data Toggle ∷ Type

foreign import data Radio ∷ Type

type DialOptions
  = { size :: Array Int
    , min :: Number
    , max :: Number
    , step :: Number
    , value :: Number
    }

type ToggleOptions
  = { size :: Array Int
    , state :: Boolean
    }

type RadioOptions
  = { active :: Int
    , numberOfButtons :: Int
    , size :: Array Int
    }


foreign import _dial
  :: EffectFn3 HTMLElement DialOptions (Number -> Effect Unit) Dial

dial
  :: HTMLElement
  -> DialOptions
  -> (Number -> Effect Unit)
  -> Effect Dial
dial element options onChangeCallback =
  runEffectFn3 _dial element options onChangeCallback


foreign import _dialWithNumber
  :: EffectFn4 HTMLElement HTMLElement DialOptions (Number -> Effect Unit) Dial

dialWithNumber
  :: HTMLElement
  -> HTMLElement
  -> DialOptions
  -> (Number -> Effect Unit)
  -> Effect Dial
dialWithNumber dialElement numberElement options onChangeCallback =
  runEffectFn4 _dialWithNumber dialElement numberElement options onChangeCallback


foreign import _updateDialValue :: EffectFn2 Dial Number Dial

updateDialValue :: Dial -> Number -> Effect Dial
updateDialValue nexusDial value = runEffectFn2 _updateDialValue nexusDial value


foreign import _toggle
  :: EffectFn3 HTMLElement ToggleOptions (Boolean -> Effect Unit) Toggle

toggle
  :: HTMLElement
  -> ToggleOptions
  -> (Boolean -> Effect Unit)
  -> Effect Toggle
toggle element options onChangeCallback =
  runEffectFn3 _toggle element options onChangeCallback


foreign import _updateToggleState :: EffectFn2 Toggle Number Toggle

updateToggleState :: Toggle -> Number -> Effect Toggle
updateToggleState nexusToggle value = runEffectFn2 _updateToggleState nexusToggle value


foreign import _radio
  :: EffectFn3 HTMLElement RadioOptions (Number -> Effect Unit) Radio

radio
  :: HTMLElement
  -> RadioOptions
  -> (Number -> Effect Unit)
  -> Effect Radio
radio element options onChangeCallback =
  runEffectFn3 _radio element options onChangeCallback
