module Capabilities.Resources.Midi where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

import Data.Midi (MidiMessage)
import ThirdParty.Socket (Socket)

class Monad m <= ManageMidi m where
  sendMidiChannel :: Socket -> Int -> m Unit

  sendMidiMessage :: Socket -> MidiMessage -> m Unit

instance manageMidiHalogenM
  :: ManageMidi m
  => ManageMidi (HalogenM st act slots msg m)
  where

  sendMidiChannel socket = lift <<< sendMidiChannel socket

  sendMidiMessage socket = lift <<< sendMidiMessage socket
