module Components.Settings where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array (sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record (merge)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

import Capabilities.LogMessage (class LogMessage)
import Capabilities.Resources.Instrument ( class ManageInstrument
                                         , createInstrument
                                         , getInstrumentsFromFile
                                         )
import Capabilities.Resources.Midi (class ManageMidi)
import Components.HigherOrder.Connect as Connect
import Components.Instrument as Instrument
import Data.Component (OpaqueSlot)
import Data.Instrument (Instrument)
import Data.Synth (Synth)
import Env (GlobalEnvironment)
import State.Global ( addInstrument
                    , setInstruments
                    , setInstrumentsFile
                    , setTrackerFile
                    )

type Slots
  = ( instrument :: OpaqueSlot Int
    )

type Input
  = { selectedInstrument :: Maybe Instrument
    | Connect.WithGlobalState () }

type State
  = { instrumentsFile :: String
    , selectedInstrument :: Maybe Instrument
    , trackerFile :: String
    | Connect.WithGlobalState ()
    }

data Action
  = AddInstrument MouseEvent
  | ChangeInstrumentsFile String
  | ChangeTrackerFile String
  | LoadInstrumentsFromFile MouseEvent String
  | LoadTrackerFromFile MouseEvent String
  | Receive { selectedInstrument :: Maybe Instrument
            | Connect.WithGlobalState ()
            }

component
  :: forall q m r
   . MonadAff m
  => LogMessage m
  => MonadAsk { globalEnvironment :: GlobalEnvironment | r } m
  => ManageInstrument m
  => ManageMidi m
  => H.Component q Input Void m
component =
  H.mkComponent
    { initialState: merge
      { instrumentsFile: "~/code/compositions/proof/instruments2.json"
      , trackerFile: "~/code/compositions/proof/tracker2.csv"
      }
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    AddInstrument mouseEvent -> do
      H.liftEffect $ preventDefault (toEvent mouseEvent)

      globalState <- H.gets _.globalState
      case globalState.synths of
        Just availableSynths -> do
          instrument <- createInstrument availableSynths
          addInstrument instrument
        Nothing -> pure unit

    ChangeInstrumentsFile file ->
      H.modify_ _ { instrumentsFile = file }

    ChangeTrackerFile file ->
      H.modify_ _ { trackerFile = file }

    LoadInstrumentsFromFile mouseEvent file -> do
      H.liftEffect $ preventDefault (toEvent mouseEvent)
      setInstrumentsFile file

      globalState <- H.gets _.globalState
      case globalState.synths of
        Just availableSynths -> do
          instruments <- getInstrumentsFromFile availableSynths file
          setInstruments instruments
        Nothing -> pure unit

    LoadTrackerFromFile mouseEvent file -> do
      H.liftEffect $ preventDefault (toEvent mouseEvent)
      setTrackerFile file

    Receive { globalState, selectedInstrument } ->
      H.modify_ _ { globalState = globalState
                  , selectedInstrument = selectedInstrument
                  }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    let
      synths = case state.globalState.synths of
        Just array -> toArray array
        Nothing -> []

      instruments = sortWith (_.number) state.globalState.instruments

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
                  [ HH.h2
                      [ HP.class_ (HH.ClassName "text-2xl") ]
                      [ HH.text "Instruments" ]
                      , HH.div_ $
                        map (renderInstrument synths) instruments
                  , HH.div
                      [ HP.class_ (HH.ClassName "mb-5") ]
                      [ HH.div
                          [ HP.class_ (HH.ClassName "flex") ]
                          [ HH.div [ HP.class_ (HH.ClassName "w-1/4") ] []
                          , HH.div [ HP.class_ (HH.ClassName "w-3/4") ]
                              [ HH.button
                                  [ HP.classes
                                    (map HH.ClassName [ "btn-green"
                                                      , "btn-normal"
                                                      ])
                                  , HE.onClick \event ->
                                      AddInstrument event
                                  ]
                                  [ HH.text "Add Instrument" ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]

  renderInstrument
    :: Array Synth
    -> Instrument
    -> H.ComponentHTML Action Slots m
  renderInstrument synths instrument =
    HH.slot
      (Proxy :: _ "instrument")
      instrument.id
      Instrument.component
      { synths, instrument }
      absurd
