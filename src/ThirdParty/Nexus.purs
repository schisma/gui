module ThirdParty.Nexus where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import Web.HTML (HTMLElement)

foreign import data Dial ∷ Type

foreign import data Radio ∷ Type

foreign import data Slider ∷ Type

foreign import data Toggle ∷ Type

type DialOptions
  = { min :: Number
    , max :: Number
    , size :: Array Int
    , step :: Number
    , value :: Number
    }

type RadioOptions
  = { active :: Int
    , numberOfButtons :: Int
    , size :: Array Int
    }

type ToggleOptions
  = { size :: Array Int
    , state :: Boolean
    }

type SliderOptions
  = { min :: Number
    , max :: Number
    , size :: Array Int
    , step :: Number
    , value :: Number
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


foreign import _radio
  :: EffectFn3 HTMLElement RadioOptions (Number -> Effect Unit) Radio

radio
  :: HTMLElement
  -> RadioOptions
  -> (Number -> Effect Unit)
  -> Effect Radio
radio element options onChangeCallback =
  runEffectFn3 _radio element options onChangeCallback


foreign import _updateRadioIndex :: EffectFn2 Radio Int Radio

updateRadioIndex :: Radio -> Int -> Effect Radio
updateRadioIndex nexusRadio index = runEffectFn2 _updateRadioIndex nexusRadio index


foreign import _slider
  :: EffectFn3 HTMLElement SliderOptions (Number -> Effect Unit) Slider

slider
  :: HTMLElement
  -> SliderOptions
  -> (Number -> Effect Unit)
  -> Effect Slider
slider element options onChangeCallback =
  runEffectFn3 _slider element options onChangeCallback


foreign import _sliderWithNumber
  :: EffectFn4 HTMLElement HTMLElement SliderOptions (Number -> Effect Unit) Slider

sliderWithNumber
  :: HTMLElement
  -> HTMLElement
  -> SliderOptions
  -> (Number -> Effect Unit)
  -> Effect Slider
sliderWithNumber sliderElement numberElement options onChangeCallback =
  runEffectFn4 _sliderWithNumber sliderElement numberElement options onChangeCallback


foreign import _updateSliderValue :: EffectFn2 Slider Number Slider

updateSliderValue :: Slider -> Number -> Effect Slider
updateSliderValue nexusSlider value = runEffectFn2 _updateSliderValue nexusSlider value


foreign import _toggle
  :: EffectFn3 HTMLElement ToggleOptions (Boolean -> Effect Unit) Toggle


toggle
  :: HTMLElement
  -> ToggleOptions
  -> (Boolean -> Effect Unit)
  -> Effect Toggle
toggle element options onChangeCallback =
  runEffectFn3 _toggle element options onChangeCallback


foreign import _updateToggleState :: EffectFn2 Toggle Boolean Toggle

updateToggleState :: Toggle -> Boolean -> Effect Toggle
updateToggleState nexusToggle value = runEffectFn2 _updateToggleState nexusToggle value
