module Components.Synths.Profit where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Midi (class ManageMidi)
import Components.Dial as Dial
import Components.Radio as Radio
import Components.Toggle as Toggle
import Data.Component (OpaqueSlot)
import Data.Instrument (Instrument)
import Data.Track (Track)
import Env (GlobalEnvironment)

type Slots
  = ( dial :: OpaqueSlot (Tuple Int String)
    , radio :: OpaqueSlot (Tuple Int String)
    , toggle :: OpaqueSlot (Tuple Int String)
    )

type Input
  = { selectedInstrument :: Instrument
    , selectedTrack :: Track
    }

type State
  = { selectedInstrument :: Instrument
    , selectedTrack :: Track
    }

data Action
  = Receive { selectedInstrument :: Instrument
            , selectedTrack :: Track
            }

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageMidi m
  => H.Component q Input Void m
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

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    Receive { selectedInstrument, selectedTrack } -> do
      H.modify_ _ { selectedInstrument = selectedInstrument
                  , selectedTrack = selectedTrack }

  renderDial
    :: State
    -> String
    -> String
    -> Array (H.ComponentHTML Action Slots m)
  renderDial state name displayName =
    let selectedTrack = state.selectedTrack
        selectedInstrument = state.selectedInstrument
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
              , selectedInstrument
              , selectedTrack
              , showNumber: true
              , size: 50
              , synthParameter
              }
              absurd
          ]
        Nothing -> []

  renderRadio
    :: State
    -> String
    -> String
    -> Array (H.ComponentHTML Action Slots m)
  renderRadio state name displayName =
    let selectedTrack = state.selectedTrack
        selectedInstrument = state.selectedInstrument
        parameters = selectedInstrument.synth.parameters
        maybeSynthParameter = find (\p -> p.name == name) parameters
    in
      case maybeSynthParameter of
        Just synthParameter ->
          [ HH.slot
              (Proxy :: _ "radio")
              (Tuple selectedInstrument.id synthParameter.name)
              Radio.component
              { displayName
              , selectedInstrument
              , selectedTrack
              , size: [180, 30]
              , synthParameter
              }
              absurd
          ]
        Nothing -> []

  renderToggle
    :: State
    -> String
    -> String
    -> Array (H.ComponentHTML Action Slots m)
  renderToggle state name displayName =
    let selectedTrack = state.selectedTrack
        selectedInstrument = state.selectedInstrument
        parameters = selectedInstrument.synth.parameters
        maybeSynthParameter = find (\p -> p.name == name) parameters
    in
      case maybeSynthParameter of
        Just synthParameter ->
          [ HH.slot
              (Proxy :: _ "toggle")
              (Tuple selectedInstrument.id synthParameter.name)
              Toggle.component
              { selectedTrack
              , selectedInstrument
              , synthParameter
              , size: 20
              , displayName
              }
              absurd
          ]
        Nothing -> []

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
        [ HP.classes (map HH.ClassName [ "mx-auto", "pt-8" ]) ]
        [ HH.div
            [ HP.classes (map HH.ClassName [ "flex"
                                           , "flex-row"
                                           , "justify-between"
                                           , "mb-5"
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
                        [ HH.h3_ [ HH.text "Poly Mod"] ]
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "polyModSourceAmountFilterEnvelope" "Filt Env")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "polyModSourceAmountOscillatorB" "Osc B")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "polyModDestinationFrequencyA" "Freq A")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "polyModDestinationPulseWidthA" "PW A")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "polyModDestinationFilter" "Filter")
                    ]
                , HH.div
                    [ HP.classes (map HH.ClassName [ "p-3"
                                                   , "border"
                                                   , "border-gray-300"
                                                   , "border-solid"
                                                   , "rounded"
                                                   , "mt-5"
                                                   ])
                    ]
                    [ HH.div
                        [ HP.class_ (HH.ClassName "text-center") ]
                        [ HH.h3_ [ HH.text "LFO"] ]
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "lfoFrequency" "Frequency")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      [ HH.div_
                        [ HH.span
                          [ HP.classes (map HH.ClassName [ "inline-block"
                                                         , "text-sm"
                                                         , "text-center"
                                                         , "width-60"
                                                         ])
                          ]
                          [ HH.text "Saw" ]
                        , HH.span
                          [ HP.classes (map HH.ClassName [ "inline-block"
                                                         , "text-sm"
                                                         , "text-center"
                                                         , "width-60"
                                                         ])
                          ]
                          [ HH.text "Triangle" ]
                        , HH.span
                          [ HP.classes (map HH.ClassName [ "inline-block"
                                                         , "text-sm"
                                                         , "text-center"
                                                         , "width-60"
                                                         ])
                          ]
                          [ HH.text "Square" ]
                        ]
                      , HH.div_
                        (renderRadio state "lfoShape" "Shape")
                      ]
                    ]
                , HH.div
                    [ HP.classes (map HH.ClassName [ "p-3"
                                                   , "border"
                                                   , "border-gray-300"
                                                   , "border-solid"
                                                   , "rounded"
                                                   , "mt-5"
                                                   ])
                    ]
                    [ HH.div
                        [ HP.class_ (HH.ClassName "text-center") ]
                        [ HH.h3_ [ HH.text "Wheel Mod"] ]
                    -- TODO: Wheel mod amount
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "wheelModSourceAmount" "LFO/Noise")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "wheelModDestinationFrequencyA" "Freq A")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "wheelModDestinationFrequencyB" "Freq B")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "wheelModDestinationPulseWidthA" "PW A")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "wheelModDestinationPulseWidthB" "PW B")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "wheelModDestinationFilter" "Filter")
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
                        [ HH.h3_ [ HH.text "Oscillator A"] ]
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "oscillatorAFrequency" "Frequency")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "oscillatorASawtooth" "Sawtooth")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "oscillatorAPulse" "Pulse")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "oscillatorAPulseWidth" "Pulse Width")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "oscillatorASync" "Sync")
                    ]
                , HH.div
                    [ HP.classes (map HH.ClassName [ "p-3"
                                                   , "border"
                                                   , "border-gray-300"
                                                   , "border-solid"
                                                   , "rounded"
                                                   , "mt-5"
                                                   ])
                    ]
                    [ HH.div
                        [ HP.class_ (HH.ClassName "text-center") ]
                        [ HH.h3_ [ HH.text "Oscillator B"] ]
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "oscillatorBFrequency" "Frequency")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "oscillatorBFineTuningCentsAdjustment" "Fine")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "oscillatorBSawtooth" "Sawtooth")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "oscillatorBTriangle" "Triangle")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "oscillatorBPulse" "Pulse")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "oscillatorBPulseWidth" "Pulse Width")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderToggle state "oscillatorBLfo" "Lo Freq")
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
                        [ HH.h3_ [ HH.text "Mixer"] ]
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "mixerOscillatorALevel" "Osc A")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "mixerOscillatorBLevel" "Osc B")
                    , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "mr-5" ])
                      ]
                      (renderDial state "mixerNoiseLevel" "Noise")
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
                        [ HH.h3_ [ HH.text "Filter"] ]
                    , HH.div
                      [ HP.classes (map HH.ClassName [ "mb-5"
                                                     , "grid"
                                                     , "grid-cols-4"
                                                     , "gap-x-3"
                                                     , "gap-y-4"
                                                     ])
                      ]
                      [ HH.div_
                        (renderDial state "filterCutoffFrequency" "Cutoff")
                      , HH.div_
                        (renderDial state "filterResonance" "Resonance")
                      , HH.div_
                        (renderDial state "filterEnvelopeAmount" "Env Amount")
                      , HH.div_
                        (renderDial state "filterKeyboardAmount" "Kbd Amount")
                      , HH.div_
                        (renderDial state "filterAttack" "Attack")
                      , HH.div_
                        (renderDial state "filterDecay" "Decay")
                      , HH.div_
                        (renderDial state "filterSustain" "Sustain")
                      , HH.div_
                        (renderDial state "filterRelease" "Release")
                      ]
                    ]
                , HH.div
                    [ HP.classes (map HH.ClassName [ "p-3"
                                                   , "border"
                                                   , "border-gray-300"
                                                   , "border-solid"
                                                   , "rounded"
                                                   , "mt-5"
                                                   ])
                    ]
                    [ HH.div
                        [ HP.class_ (HH.ClassName "text-center") ]
                        [ HH.h3_ [ HH.text "Amplifier"] ]
                    , HH.div
                      [ HP.classes (map HH.ClassName [ "mb-5"
                                                     , "grid"
                                                     , "grid-cols-4"
                                                     , "gap-x-3"
                                                     , "gap-y-4"
                                                     ])
                      ]
                      [ HH.div_
                        (renderDial state "amplifierAttack" "Attack")
                      , HH.div_
                        (renderDial state "amplifierDecay" "Decay")
                      , HH.div_
                        (renderDial state "amplifierSustain" "Sustain")
                      , HH.div_
                        (renderDial state "amplifierRelease" "Release")
                      ]
                    ]
                ]
            ]
        ]
