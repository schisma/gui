module Components.Settings where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array (sortWith)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Instrument (class ManageInstrument)
import Capabilities.Resources.Midi (class ManageMidi)
import Components.Instrument as Instrument
import Data.Instrument (Instrument)
import Data.Synth (Synth)
import Env (GlobalEnvironment)
import Svg.Icons (iconPlus)

type Slots
  = ( instrument :: H.Slot (Const Void) Instrument.Output UUID
    )

type Input
  = { instruments :: Array Instrument
    , instrumentsFile :: String
    , selectedInstrument :: Maybe Instrument
    , synths :: NonEmptyArray Synth
    , trackerFile :: String
    }

type State
  = { instruments :: Array Instrument
    , instrumentsFile :: String
    , selectedInstrument :: Maybe Instrument
    , synths :: NonEmptyArray Synth
    , trackerFile :: String
    }

data Action
  = AddInstrument MouseEvent
  | ChangeInstrumentsFile String
  | ChangeTrackerFile String
  | HandleInstrument Instrument.Output
  | LoadInstrumentsFromFile MouseEvent String
  | LoadTrackerFromFile MouseEvent String
  | Receive { instruments :: Array Instrument
            , instrumentsFile :: String
            , selectedInstrument :: Maybe Instrument
            , synths :: NonEmptyArray Synth
            , trackerFile :: String
            }

data Output
  = AddedInstrument
  | ChangedInstrumentsFile String
  | ChangedTrackerFile String
  | ClonedInstrument Instrument
  | RemovedInstrument Instrument
  | UpdatedInstrument Instrument

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageInstrument m
  => ManageMidi m
  => H.Component q Input Output m
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

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    AddInstrument mouseEvent -> do
      H.liftEffect $ preventDefault (toEvent mouseEvent)
      H.raise AddedInstrument

    ChangeInstrumentsFile file ->
      H.raise (ChangedInstrumentsFile file)

    ChangeTrackerFile file -> do
      H.raise (ChangedTrackerFile file)

    HandleInstrument output ->
      case output of
        Instrument.ClonedInstrument instrument ->
          H.raise (ClonedInstrument instrument)

        Instrument.RemovedInstrument instrument ->
          H.raise (RemovedInstrument instrument)

        Instrument.UpdatedInstrument instrument ->
          H.raise (UpdatedInstrument instrument)

    LoadInstrumentsFromFile mouseEvent file -> do
      H.liftEffect $ preventDefault (toEvent mouseEvent)
      H.raise (ChangedInstrumentsFile file)

    LoadTrackerFromFile mouseEvent file -> do
      H.liftEffect $ preventDefault (toEvent mouseEvent)
      H.raise (ChangedTrackerFile file)

    Receive record ->
      H.put record

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    let instruments = sortWith (_.number) state.instruments

    in  HH.div
          [ HP.class_ (HH.ClassName "panel") ]
          [ HH.div
              [ HP.class_ (HH.ClassName "flex") ]
              [ HH.div
                  [ HP.classes (map HH.ClassName [ "overflow-y-auto"
                                                  , "p-5"
                                                  , "w-1/2"
                                                  ])
                  ]
                  [ HH.h2
                      [ HP.classes (map HH.ClassName [ "text-2xl"
                                                      , "mb-4"
                                                      ])
                      ]
                      [ HH.text "Files" ]
                  , HH.div
                      [ HP.class_ (HH.ClassName "mb-5") ]
                      [ HH.form
                          [ HP.class_ (HH.ClassName "mb-2") ]
                          [ HH.label
                              [ HP.classes
                                (map HH.ClassName [ "flex"
                                                  , "items-center"
                                                  ])
                              ]
                              [ HH.span
                                [ HP.class_ (HH.ClassName "w-1/4") ]
                                [ HH.text "Instruments File" ]
                              , HH.div
                                  [ HP.classes
                                    (map HH.ClassName [ "flex", "w-3/4" ])
                                  ]
                                  [ HH.input
                                      [ HP.classes
                                        (map HH.ClassName [ "input"
                                                          , "inline-flex"
                                                          , "mr-2"
                                                          ])
                                      , HP.type_ HP.InputText
                                      , HP.name "file"
                                      , HP.value state.instrumentsFile
                                      , HE.onValueChange
                                        ChangeInstrumentsFile
                                      ]
                                  , HH.button
                                      [ HP.classes
                                        (map HH.ClassName [ "btn-white"
                                                          , "btn-normal"
                                                          ])
                                      , HE.onClick \event ->
                                          LoadInstrumentsFromFile
                                          event
                                          state.instrumentsFile
                                      ]
                                      [ HH.text "Load" ]
                                  ]
                              ]
                          ]
                      , HH.form
                          [ HP.class_ (HH.ClassName "mb-2") ]
                          [ HH.label
                              [ HP.classes
                                (map HH.ClassName [ "flex"
                                                  , "items-center"
                                                  ])
                              ]
                              [ HH.span
                                [ HP.class_ (HH.ClassName "w-1/4") ]
                                [ HH.text "Tracker File" ]
                              , HH.div
                                  [ HP.classes
                                    (map HH.ClassName [ "flex", "w-3/4" ])
                                  ]
                                  [ HH.input
                                      [ HP.classes
                                        (map HH.ClassName [ "input"
                                                          , "inline-flex"
                                                          , "mr-2"
                                                          ])
                                      , HP.type_ HP.InputText
                                      , HP.name "file"
                                      , HP.value state.trackerFile
                                      , HE.onValueChange ChangeTrackerFile
                                      ]
                                  , HH.button
                                      [ HP.classes
                                        (map HH.ClassName [ "btn-white"
                                                          , "btn-normal"
                                                          ])
                                      , HE.onClick \event ->
                                          LoadTrackerFromFile
                                          event
                                          state.trackerFile
                                      ]
                                      [ HH.text "Load" ]
                                  ]
                              ]
                          ]
                      ]
                  ]
              , HH.form
                  [ HP.classes (map HH.ClassName [ "overflow-y-auto"
                                                  , "p-5"
                                                  , "w-1/2"
                                                  ])
                  ]
                  [ HH.div
                    [ HP.classes (map HH.ClassName [ "flex"
                                                   , "items-center"
                                                   ])
                    ]
                    [ HH.div
                      [ HP.class_ (HH.ClassName "w-3/4") ]
                      [ HH.h2
                        [ HP.class_ (HH.ClassName "text-2xl") ]
                        [ HH.text "Instruments" ]
                      ]
                    , HH.div
                      [ HP.class_ (HH.ClassName "w-1/4") ]
                      [ HH.div
                        [ HP.classes (map HH.ClassName [ "flex"
                                                       , "justify-end"
                                                       ])
                        ]
                        [ HH.button
                          [ HP.classes
                            (map HH.ClassName [ "btn-green"
                                              , "btn-normal"
                                              ])
                          , HE.onClick \event ->
                              AddInstrument event
                          ]
                          [ HH.div
                              [ HP.classes
                                (map HH.ClassName [ "inline-block"
                                                  , "align-middle"
                                                  , "mr-1"
                                                  ])
                              ]
                              [ iconPlus
                                  []
                              ]
                          , HH.div
                              [ HP.classes
                                (map HH.ClassName [ "inline-block"
                                                  , "align-middle"
                                                  ])
                              ]
                              [ HH.text "Add" ]
                          ]
                        ]
                      ]
                    ]
                  , HH.div_ $ map (renderInstrument state.synths) instruments
                  ]
              ]
          ]

  renderInstrument
    :: NonEmptyArray Synth
    -> Instrument
    -> H.ComponentHTML Action Slots m
  renderInstrument synths instrument =
    HH.slot
      (Proxy :: _ "instrument")
      instrument.id
      Instrument.component
      { synths, instrument }
      HandleInstrument
