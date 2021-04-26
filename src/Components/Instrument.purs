module Components.Instrument where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Type.Proxy (Proxy(..))

import Capabilities.LogMessage (class LogMessage)
import Data.Instrument (Instrument, updateSynth)
import Data.Midi (availableChannels)
import Data.Synth (Synth)
import Env (GlobalEnvironment)
import State.Global (addInstrument, removeInstrument, updateInstrument)
import Svg.Icons (iconCopy, iconSelectArrow, iconTrash)

type Input
  = { synths :: Array Synth
    , instrument :: Instrument
    }

type State
  = { synths :: Array Synth
    , instrument :: Instrument
    , availableMidiChannels :: Array Int
    }

type Slots :: forall k. Row k
type Slots = ()

data ChangedField
  = FieldMidiChannel String
  | FieldName String
  | FieldPath String
  | FieldSynth String

data Action
  = ChangeField Instrument ChangedField
  | Clone Instrument
  | Receive { synths :: Array Synth, instrument :: Instrument }
  | Remove Instrument

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => H.Component q Input Void m
component =
  H.mkComponent
    { initialState:
        Record.insert
        (Proxy :: _ "availableMidiChannels")
        availableChannels
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    ChangeField instrument field -> do
      let updatedInstrument = case field of
            FieldName name -> instrument { name = name }
            FieldSynth synthName -> updateSynth instrument synthName
            FieldPath path -> instrument { soundFontPath = path }
            FieldMidiChannel channel ->
              case fromString channel of
                Nothing -> instrument
                Just number -> instrument { midiChannel = number }

      updateInstrument updatedInstrument

    Clone instrument ->
      addInstrument instrument

    Receive { synths, instrument } ->
      H.modify_ _ { synths = synths, instrument = instrument }

    Remove instrument ->
      removeInstrument instrument

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    let pathInputClasses =
          if state.instrument.synth.name == "SoundFont"
            then [ "mb-2" ]
            else [ "mb-2", "hidden" ]
    in
        HH.div
          [ HP.classes (map HH.ClassName [ "instrument", "my-5" ]) ]
          [ HH.div
              [ HP.class_ (HH.ClassName "mb-2") ]
              [ HH.label
                  [ HP.classes (map HH.ClassName [ "flex", "items-center" ]) ]
                  [ HH.span
                    [ HP.class_ (HH.ClassName "w-1/4") ]
                    [ HH.text "Name" ]
                  , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "w-3/4" ])
                      ]
                      [ HH.input
                          [ HP.class_ (HH.ClassName "input")
                          , HP.value state.instrument.name
                          , HP.type_ HP.InputText
                          , HE.onValueChange
                              (ChangeField state.instrument <<< FieldName)
                          ]
                      ]
                  ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "mb-2") ]
              [ HH.label
                  [ HP.classes (map HH.ClassName [ "flex", "items-center" ]) ]
                  [ HH.span
                    [ HP.class_ (HH.ClassName "w-1/4") ]
                    [ HH.text "Synth" ]
                  , HH.div
                      [ HP.classes (map HH.ClassName [ "inline-block"
                                                     , "relative"
                                                     , "w-3/4"
                                                     ])
                      ]
                      [ HH.select
                          [ HP.class_ (HH.ClassName "input")
                          , HP.value state.instrument.synth.name
                          , HE.onValueChange
                              (ChangeField state.instrument <<< FieldSynth)
                          ]
                          (map renderSynth state.synths)
                      , HH.div
                          [ HP.class_ (HH.ClassName "select-arrow") ]
                          [ iconSelectArrow
                            [ HH.attr
                              (HH.AttrName "class")
                              "fill-current h-4 w-4"
                            ]
                          ]
                      ]
                  ]
              ]
          , HH.div
              [ HP.classes (map HH.ClassName pathInputClasses) ]
              [ HH.label
                  [ HP.classes (map HH.ClassName [ "flex", "items-center" ]) ]
                  [ HH.span
                    [ HP.class_ (HH.ClassName "w-1/4") ]
                    [ HH.text "Path" ]
                  , HH.div
                      [ HP.classes
                        (map HH.ClassName [ "inline-block", "w-3/4" ])
                      ]
                      [ HH.input
                          [ HP.class_ (HH.ClassName "input")
                          , HP.type_ HP.InputText
                          , HP.value state.instrument.soundFontPath
                          , HE.onValueChange
                              (ChangeField state.instrument <<< FieldPath)
                          ]
                      ]
                  ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "mb-4") ]
              [ HH.label
                  [ HP.classes (map HH.ClassName [ "flex", "items-center" ]) ]
                  [ HH.span
                    [ HP.class_ (HH.ClassName "w-1/4") ]
                    [ HH.text "MIDI Channel" ]
                  , HH.div
                      [ HP.classes (map HH.ClassName [ "inline-block"
                                                     , "relative"
                                                     , "w-3/4"
                                                     ])
                      ]
                      [ HH.select
                          [ HP.class_ (HH.ClassName "input")
                          , HP.value $ show state.instrument.midiChannel
                          , HE.onValueChange
                              (ChangeField state.instrument <<< FieldMidiChannel)
                          ]
                          (map renderMidiChannel state.availableMidiChannels)
                      , HH.div
                          [ HP.class_ (HH.ClassName "select-arrow") ]
                          [ iconSelectArrow
                            [ HH.attr
                              (HH.AttrName "class")
                              "fill-current h-4 w-4"
                            ]
                          ]
                      ]
                  ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "mb-4") ]
              [ HH.div
                  [ HP.class_ (HH.ClassName "flex") ]
                  [ HH.span [ HP.class_ (HH.ClassName "w-1/4") ] []
                  , HH.div
                    [ HP.classes
                      (map HH.ClassName [ "inline-block", "w-3/4"])
                    ]
                    [ HH.div
                        [ HP.classes
                          (map HH.ClassName [ "inline-block", "w-1/3" ])
                        ]
                        [ HH.div
                            [ HP.class_ (HH.ClassName "text-center") ]
                            [ HH.button
                                [ HP.classes (map HH.ClassName [ "btn-blue"
                                                               , "btn-normal"
                                                               ])
                                , HE.onClick \_ -> (Clone state.instrument)
                                ]
                                [ HH.div
                                    [ HP.classes
                                      (map HH.ClassName [ "inline-block"
                                                        , "align-middle"
                                                        , "mr-1"
                                                        ])
                                    ]
                                    [ iconCopy
                                        []
                                    ]
                                , HH.div
                                    [ HP.classes
                                      (map HH.ClassName [ "inline-block"
                                                        , "align-middle"
                                                        ])
                                    ]
                                    [ HH.text "Clone" ]
                                ]
                            ]
                        ]
                    , HH.div
                        [ HP.classes (map HH.ClassName [ "inline-block"
                                                       , "w-1/3"
                                                       , "text-center"
                                                       , "text-2xl"
                                                       , "align-middle"
                                                       , "text-gray-300"
                                                       ]) ]
                        [ HH.text $ show state.instrument.number ]
                    , HH.div
                        [ HP.classes
                          (map HH.ClassName [ "inline-block", "w-1/3" ])
                        ]
                        [ HH.div
                            [ HP.class_ (HH.ClassName "text-center") ]
                            [ HH.button
                                [ HP.classes (map HH.ClassName [ "btn-red"
                                                               , "btn-normal"
                                                               ])
                                , HE.onClick \_ -> (Remove state.instrument)
                                ]
                                [ HH.div
                                    [ HP.classes
                                      (map HH.ClassName [ "inline-block"
                                                        , "align-middle"
                                                        , "mr-1"
                                                        ])
                                    ]
                                    [ iconTrash
                                        []
                                    ]
                                , HH.div
                                    [ HP.classes
                                      (map HH.ClassName [ "inline-block"
                                                        , "align-middle"
                                                        ])
                                    ]
                                    [ HH.text "Remove" ]
                                ]
                            ]
                        ]
                      ]
                  ]
              ]
          , HH.hr []
          ]

  renderMidiChannel :: Int -> H.ComponentHTML Action Slots m
  renderMidiChannel channel =
    let value = show channel
        name = if channel == 0 then "0 (Off)" else value
    in HH.option [ HP.value value ] [ HH.text name ]

  renderSynth :: Synth -> H.ComponentHTML Action Slots m
  renderSynth { name } =
    HH.option
      [ HP.value name ]
      [ HH.text name ]
