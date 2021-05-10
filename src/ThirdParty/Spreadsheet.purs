module ThirdParty.Spreadsheet where

import Prelude

import Data.Function.Uncurried (Fn1, mkFn1)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import data Spreadsheet ∷ Type

type Callbacks
  = { afterChange :: Spreadsheet -> Effect Unit
    , afterCreateCol :: Spreadsheet -> Int -> Effect Unit
    , afterCreateRow :: Spreadsheet -> Effect Unit
    , afterRemoveCol :: Spreadsheet -> Array Int -> Effect Unit
    , afterRemoveRow :: Spreadsheet -> Effect Unit
    , onBlur :: Spreadsheet -> Effect Unit
    , onMute :: Spreadsheet -> Effect Unit
    , onPlay :: Spreadsheet -> Effect Unit
    , onPlayOnlyMidi :: Spreadsheet -> Effect Unit
    , onSelection :: Spreadsheet -> Array Int -> Effect Unit
    , onSolo :: Spreadsheet -> Effect Unit
    , onStop :: Spreadsheet -> Effect Unit
    }

type RowNumbers
  = { start :: Int
    , end :: Int
    }


foreign import exportAsCsv ∷ Spreadsheet -> String


foreign import _rowNumbers :: Fn1 Spreadsheet RowNumbers

rowNumbers :: Spreadsheet -> RowNumbers
rowNumbers sheet = mkFn1 _rowNumbers sheet


foreign import _spreadsheet ∷ EffectFn2 String Callbacks Spreadsheet

spreadsheet :: String -> Callbacks -> Effect Spreadsheet
spreadsheet element callbacks = runEffectFn2 _spreadsheet element callbacks


foreign import _updateSpreadsheetData ∷ EffectFn2 Spreadsheet (Array (Array String)) Spreadsheet

updateData :: Spreadsheet -> Array (Array String) -> Effect Spreadsheet
updateData sheet rows = runEffectFn2 _updateSpreadsheetData sheet rows


foreign import _updateSpreadsheetHeaders ∷ EffectFn2 Spreadsheet (Array String) Spreadsheet

updateHeaders :: Spreadsheet -> Array String -> Effect Spreadsheet
updateHeaders sheet headers = runEffectFn2 _updateSpreadsheetHeaders sheet headers
