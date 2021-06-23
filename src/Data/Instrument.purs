module Data.Instrument where

import Prelude
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, find, head)
import Data.Int (floor)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Foreign.Object (Object, fromFoldable, toUnfoldable)

import Data.Midi (MidiMessage, midiMessage)
import Data.Synth (Synth, SynthParameter, updateSynthParameters)
import Data.Utilities (scale)

type Instrument
  = { availableSynths :: NonEmptyArray Synth
    , id :: UUID
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

fromInstrumentJson
  :: NonEmptyArray Synth
  -> UUID
  -> InstrumentJson
  -> Instrument
fromInstrumentJson availableSynths id json =
  let synth = case find (\s -> s.name == json.instrument) availableSynths of
                        Nothing -> head availableSynths
                        Just s -> s

      parameters = Map.fromFoldable $ (toUnfoldable json.parameters :: Array _)

  in  { availableSynths: availableSynths
      , id
      , midiChannel: json.midiChannel
      , name: json.name
      , number: json.number
      , soundFontPath: json.soundFontPath
      , synth: updateSynthParameters synth parameters
      }

toInstrumentJson :: Instrument -> InstrumentJson
toInstrumentJson instrument =
  { instrument: instrument.synth.name
  , midiChannel: instrument.midiChannel
  , name: instrument.name
  , number: instrument.number
  , parameters: fromFoldable $
                map (\p -> Tuple p.name p.value) instrument.synth.parameters
  , soundFontPath: instrument.soundFontPath
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

new :: NonEmptyArray Synth -> UUID -> Int -> Instrument
new availableSynths id number =
  { availableSynths
  , id
  , midiChannel: 0
  , name: "New Instrument"
  , number
  , soundFontPath: ""
  , synth: head availableSynths
  }

remove :: Array Instrument -> Instrument -> Array Instrument
remove instruments instrumentToRemove =
  let filteredInstruments = Array.filter ((/=) instrumentToRemove) instruments
  in  Array.mapWithIndex
        (\index instr -> instr { number = index + 1 })
        $ Array.sortWith (_.number) filteredInstruments

updateSynth :: Instrument -> String -> Instrument
updateSynth instrument synthName =
  case find (\synth -> synth.name == synthName) instrument.availableSynths of
    Nothing -> instrument
    Just synth -> instrument { synth = synth }

updateSynthParameterValue :: Instrument -> SynthParameter -> Instrument
updateSynthParameterValue instrument synthParameter =
  let parameter = Map.singleton synthParameter.name synthParameter.value
      synth = updateSynthParameters instrument.synth parameter
  in  instrument { synth = synth }
