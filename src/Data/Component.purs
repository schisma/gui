module Data.Component where

import Prelude

import Halogen (Slot)

type OpaqueSlot slot = forall query. Slot query Void slot
