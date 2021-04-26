module ThirdParty.Socket where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

import Data.Midi (MidiMessage)

foreign import data Socket ∷ Type


foreign import _createSocket ∷ EffectFn1 Int Socket

createSocket :: Int -> Effect Socket
createSocket port = runEffectFn1 _createSocket port


foreign import _sendMidiChannel ∷ EffectFn2 Socket Int Unit

sendMidiChannel :: Socket -> Int -> Effect Unit
sendMidiChannel socket channel = runEffectFn2 _sendMidiChannel socket channel


foreign import _sendMidiMessage ∷ EffectFn2 Socket MidiMessage Unit

sendMidiMessage :: Socket -> MidiMessage -> Effect Unit
sendMidiMessage socket message = runEffectFn2 _sendMidiMessage socket message
