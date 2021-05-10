module Data.Track
  ( Track(..)
  , fromTrackerData
  , toggleMute
  , toggleSolo
  , toCsvName
  , toInstrument
  , toName
  )
where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes, find, head)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID (UUID)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodePoints (char, regex, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (choice, optionMaybe)

import Data.Instrument (Instrument)

data Track
  = MasterTrack
  | LineNumberTrack
  | InstrumentTrack
    { instrumentId :: UUID
    , mute :: Boolean
    , name :: String
    , solo :: Boolean
    }

derive instance ordTrack :: Ord Track
derive instance eqTrack :: Eq Track

data TrackHeader
  = MasterHeader
  | LineNumberHeader
  | InstrumentHeader Int String Boolean Boolean

toggleMute :: Track -> Track
toggleMute (InstrumentTrack t) = InstrumentTrack $ t { mute = not t.mute }
toggleMute t = t

toggleSolo :: Track -> Track
toggleSolo (InstrumentTrack t) = InstrumentTrack $ t { solo = not t.solo }
toggleSolo t = t

toCsvName :: Array Instrument -> Track -> String
toCsvName instruments track = case track of
  MasterTrack -> "Master"
  LineNumberTrack -> "#"
  InstrumentTrack instrumentTrack ->
    let prefix =
          if instrumentTrack.mute then "M"
          else if instrumentTrack.solo then "S"
          else ""
        maybeInstrument = toInstrument instruments track
        instrumentNumber = case maybeInstrument of
                             Nothing -> "0"
                             Just instrument -> show instrument.number
     in  prefix <> "I" <> instrumentNumber <> " " <> instrumentTrack.name

toInstrument :: Array Instrument -> Track -> Maybe Instrument
toInstrument instruments track =
  case track of
    InstrumentTrack instrumentTrack ->
      find
      (\instrument -> instrument.id == instrumentTrack.instrumentId)
      instruments
    _ -> Nothing

toName :: Track -> String
toName = case _ of
  MasterTrack -> "Master"
  LineNumberTrack -> "#"
  InstrumentTrack track ->
    let prefix =
          if track.mute then "(M) "
          else if track.solo then "(S) "
          else ""
    in  prefix <> track.name

fromTrackerData :: Array Instrument -> Array (Array String) -> Array Track
fromTrackerData instruments tracker =
  case head tracker of
    Nothing -> []
    Just row -> catMaybes $ map (parseTrackName instruments) row

parseTrackName :: Array Instrument -> String -> Maybe Track
parseTrackName instruments trackName =
  case runParser parseTrackHeader trackName of
    Left error -> Nothing
    Right trackHeader ->
      case trackHeader of
        MasterHeader -> Just MasterTrack
        LineNumberHeader -> Just LineNumberTrack
        InstrumentHeader number name mute solo ->
          case find (\i -> i.number == number) instruments of
            Nothing -> Nothing
            Just instrument -> Just $
              InstrumentTrack { instrumentId: instrument.id
                              , mute
                              , name
                              , solo
                              }

parseTrackHeader :: Parser TrackHeader
parseTrackHeader =
  parseLineNumberTrack <|> parseMasterTrack <|> parseInstrumentTrackName

parseLineNumberTrack :: Parser TrackHeader
parseLineNumberTrack = do
  void $ string "#"
  pure LineNumberHeader

parseMasterTrack :: Parser TrackHeader
parseMasterTrack = do
  void $ string "Master"
  pure MasterHeader

parseInstrumentTrackName :: Parser TrackHeader
parseInstrumentTrackName = do
  muteOrSolo <- optionMaybe $ choice [char 'M', char 'S']
  void $ char 'I'
  num <- regex "[0-9]+"
  instrumentName <- optionMaybe do
    skipSpaces
    regex ".*"

  let name = fromMaybe "" instrumentName
      number = fromMaybe (-1) $ fromString num
      { mute, solo } = case muteOrSolo of
        Just 'M' -> { mute: true, solo: false }
        Just 'S' -> { mute: false, solo: true }
        _        -> { mute: false, solo: false }

  pure (InstrumentHeader number name mute solo)
