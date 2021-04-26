module Data.Midi where

import Prelude

import Data.Array((..))

import ThirdParty.Standard (Uint8Array, toUint8Array)

type MidiMessage
  = { data :: Uint8Array
    , timestamp :: Int
    }

allowedCommands :: Array Int
allowedCommands =
  [ 128 -- note off
  , 144 -- note on
  , 176 -- control change
  ]

availableChannels :: Array Int
availableChannels = 0..16

midiMessage :: Int -> Int -> Int -> Int -> MidiMessage
midiMessage command noteNumber value channel =
  { data: toUint8Array [command + channel - 1, noteNumber, value]
  , timestamp: 0
  }
