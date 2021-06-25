module Components.Synths.SoundFont where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array (find)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Midi (class ManageMidi)
import Components.Dial as Dial
import Data.Component (SynthControlOutput)
import Data.Instrument (Instrument)
import Env (GlobalEnvironment)

type Slots
  = ( dial :: H.Slot (Const Void) SynthControlOutput (Tuple UUID String)
    )

type Input
  = { selectedInstrument :: Instrument
    }

type State
  = { selectedInstrument :: Instrument
    }

data Action
  = HandleSynthControl SynthControlOutput
  | Receive { selectedInstrument :: Instrument }

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageMidi m
  => H.Component q Input SynthControlOutput m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots SynthControlOutput m Unit
  handleAction = case _ of
    HandleSynthControl output -> H.raise output

    Receive { selectedInstrument } -> do
      H.modify_ _ { selectedInstrument = selectedInstrument }

  renderDial
    :: State
    -> String
    -> String
    -> Array (H.ComponentHTML Action Slots m)
  renderDial state name displayName =
    let selectedInstrument = state.selectedInstrument
        parameters = selectedInstrument.synth.parameters
        maybeSynthParameter = find (\p -> p.name == name) parameters
    in
      case maybeSynthParameter of
        Just synthParameter ->
          [ HH.slot
              (Proxy :: _ "dial")
              (Tuple selectedInstrument.id synthParameter.name)
              Dial.component
              { displayName
              , showNumber: true
              , size: 50
              , synthParameter
              }
              HandleSynthControl
          ]
        Nothing -> []

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
        [ HP.classes (map HH.ClassName [ "mx-auto", "pt-8" ]) ]
        [ HH.div
            [ HP.classes (map HH.ClassName [ "flex"
                                           , "flex-row"
                                           , "justify-center"
                                           ])
            ]
            [ HH.div
                [ HP.classes (map HH.ClassName [ "flex"
                                               , "flex-col"
                                               , "mr-5"
                                               ])
                ]
                [ HH.div
                    [ HP.classes (map HH.ClassName [ "p-3"
                                                   , "border"
                                                   , "border-gray-300"
                                                   , "border-solid"
                                                   , "rounded"
                                                   ])
                    ]
                    [ HH.div
                        [ HP.class_ (HH.ClassName "text-center") ]
                        [ HH.h3_ [ HH.text "SoundFont"] ]
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "bank" "Bank")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "program" "Program")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block" ])
                      ]
                      (renderDial state "velocity" "Velocity")
                    ]
                ]
            , HH.div
                [ HP.classes (map HH.ClassName [ "flex"
                                               , "flex-col"
                                               , "mr-5"
                                               ])
                ]
                [ HH.div
                    [ HP.classes (map HH.ClassName [ "p-3"
                                                   , "border"
                                                   , "border-gray-300"
                                                   , "border-solid"
                                                   , "rounded"
                                                   ])
                    ]
                    [ HH.div
                        [ HP.class_ (HH.ClassName "text-center") ]
                        [ HH.h3_ [ HH.text "Amplifier"] ]
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "amplifierAttack" "Attack")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "amplifierDecay" "Decay")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "amplifierSustain" "Sustain")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block" ])
                      ]
                      (renderDial state "amplifierRelease" "Release")
                    ]
                ]
            , HH.div
                [ HP.classes (map HH.ClassName [ "flex", "flex-col" ]) ]
                [ HH.div
                    [ HP.classes (map HH.ClassName [ "p-3"
                                                   , "border"
                                                   , "border-gray-300"
                                                   , "border-solid"
                                                   , "rounded"
                                                   ])
                    ]
                    [ HH.div
                        [ HP.class_ (HH.ClassName "text-center") ]
                        [ HH.h3_ [ HH.text "Misc."] ]
                    , HH.div
                      [ HP.class_ (HH.ClassName "inline-block") ]
                      (renderDial state "amplitude" "Volume")
                    ]
                ]
            ]
        ]
