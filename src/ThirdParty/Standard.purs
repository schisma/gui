module ThirdParty.Standard where

foreign import data Uint8Array :: Type

foreign import toUint8Array :: Array Int -> Uint8Array
