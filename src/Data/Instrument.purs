module Data.Instrument where

import Prelude
import Data.Array(catMaybes)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, find, head)
import Data.Int (floor)
import Data.Map (fromFoldable, singleton)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object, toUnfoldable)

import Data.Midi(MidiMessage, midiMessage)
import Data.Synth(Synth, SynthParameter, updateSynthParameters)
import Data.Utilities(scale)

type Instrument
  = { availableSynths :: NonEmptyArray Synth
    , id :: Int
    , midiChannel :: Int
    , name :: String
    , number :: Int
    , soundFontPath :: String
    , synth :: Synth
    }

type InstrumentJson
  = { instrument :: String
    , midiChannel :: Int
    , name :: String
    , number :: Int
    , parameters :: Object Number
    , soundFontPath :: String
    }

fromInstrumentJson :: NonEmptyArray Synth -> InstrumentJson -> Instrument
fromInstrumentJson availableSynths json =
  let synth = case find (\s -> s.name == json.instrument) availableSynths of
                        Nothing -> head availableSynths
                        Just s -> s

      parameters = fromFoldable $ (toUnfoldable json.parameters :: Array _)

  in  { availableSynths: availableSynths
      , id: 0
      , midiChannel: json.midiChannel
      , name: json.name
      , number: json.number
      , soundFontPath: json.soundFontPath
      , synth: updateSynthParameters synth parameters
      }

midiControlChangeMessage :: Instrument -> SynthParameter -> Maybe MidiMessage
midiControlChangeMessage instrument synthParameter =
  case Array.find ((==) synthParameter) instrument.synth.parameters of
    Nothing -> Nothing
    Just parameter ->
      let value = floor $
                  scale
                  parameter.value
                  parameter.minimum
                  parameter.maximum
                  0.0
                  127.0
          channel = instrument.midiChannel
      in  Just $ midiMessage 176 parameter.midiCcNumber value channel

midiControlChangeMessages :: Instrument -> Array MidiMessage
midiControlChangeMessages instrument =
  catMaybes $
    map (midiControlChangeMessage instrument) instrument.synth.parameters

updateSynth :: Instrument -> String -> Instrument
updateSynth instrument synthName =
  case find (\synth -> synth.name == synthName) instrument.availableSynths of
    Nothing -> instrument
    Just synth -> instrument { synth = synth }

updateSynthParameterValue :: Instrument -> SynthParameter -> Instrument
updateSynthParameterValue instrument synthParameter =
  let parameter = singleton synthParameter.name synthParameter.value
      synth = updateSynthParameters instrument.synth parameter
  in  instrument { synth = synth }
