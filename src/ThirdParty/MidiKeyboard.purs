module ThirdParty.MidiKeyboard where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Web.HTML (HTMLElement)

type MidiKeyboardMessage
  = { command :: Int
    , noteNumber :: Int
    , value :: Int
    }

foreign import data MidiKeyboard ∷ Type

foreign import _midiKeyboard :: EffectFn2 HTMLElement (MidiKeyboardMessage -> Effect Unit) MidiKeyboard

midiKeyboard :: HTMLElement -> (MidiKeyboardMessage -> Effect Unit) -> Effect MidiKeyboard
midiKeyboard element callback = runEffectFn2 _midiKeyboard element callback
