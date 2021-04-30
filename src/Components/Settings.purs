module Components.Settings where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array (sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
import Components.Synth as Synth
import Data.Component (OpaqueSlot)
import Data.Instrument (Instrument)
import Data.Synth (Synth)
import Data.Track (Track(..))
import Env (GlobalEnvironment, GlobalState)
import State.Global ( addInstrument
                    , getSelectedTrackAndInstrument
                    , setInstruments
                    , setInstrumentsFile
                    , setTrackerFile
                    )

type Slots
  = ( instrument :: OpaqueSlot Int
    , synth :: OpaqueSlot Unit
    )

data Panel
  = Configuration
  | SynthControls

derive instance eqPanel :: Eq Panel

type Input
  = { | Connect.WithGlobalState () }

type State
  = { displayedPanel :: Panel
    , instrumentsFile :: String
    , trackerFile :: String
    | Connect.WithGlobalState ()
    }

data Action
  = AddInstrument MouseEvent
  | ChangeInstrumentsFile String
  | ChangeTrackerFile String
  | ClickPanel Panel
  | LoadInstrumentsFromFile MouseEvent String
  | LoadTrackerFromFile MouseEvent String
  | Receive { | Connect.WithGlobalState () }

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
    { initialState: \{ globalState } ->
      { displayedPanel: Configuration
      , instrumentsFile: "~/code/compositions/proof/instruments2.json"
      , trackerFile: "~/code/compositions/proof/tracker2.csv"
      , globalState
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

    ClickPanel panel ->
      H.modify_ _ { displayedPanel = panel }

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

    Receive { globalState } ->
      H.modify_ _ { globalState = globalState }

  linkName :: Panel -> String
  linkName panel = case panel of
    Configuration -> "Configuration"
    SynthControls -> "Synth Controls"

  panelClasses :: State -> Panel -> Array HH.ClassName
  panelClasses state panel =
    let
      classes =
        if panel == state.displayedPanel then
          [ "panel" ]
        else
          [ "panel", "hidden" ]
    in
      map HH.ClassName classes

  panelLinkClasses :: State -> Panel -> Array HH.ClassName
  panelLinkClasses state panel =
    let
      classes =
        if panel == state.displayedPanel then
          [ "nav-link", "active" ]
        else
          [ "nav-link" ]
    in
      map HH.ClassName classes

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    let
      synths = case state.globalState.synths of
        Just array -> toArray array
        Nothing -> []

      instruments = sortWith (_.number) state.globalState.instruments

    in HH.div
      [ HP.class_ (HH.ClassName "mt-2") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "nav") ]
          [ HH.ul
              [ HP.class_ (HH.ClassName "flex") ]
              [ renderPanelLink state Configuration
              , renderPanelLink state SynthControls
              ]
          ]
      , HH.div
          [ HP.class_ (HH.ClassName "settings") ]
          [ HH.div
              [ HP.classes (panelClasses state Configuration) ]
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
          , HH.div
              [ HP.classes (panelClasses state SynthControls)
              ]
              [ HH.div
                  [ HP.class_ (HH.ClassName "synth-controls") ]
                  (renderSynth state.globalState)
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

  renderPanelLink :: State -> Panel -> H.ComponentHTML Action Slots m
  renderPanelLink state panel =
    HH.li
      [ HP.classes (map HH.ClassName [ "flex-1", "mr-2" ])
      , HE.onClick \_ -> ClickPanel panel
      ]
      [ HH.a
          [ HP.classes $ panelLinkClasses state panel
          , HP.href "#"
          ]
          [ HH.text $ linkName panel ]
      ]

  renderSynth :: GlobalState -> Array (H.ComponentHTML Action Slots m)
  renderSynth globalState =
    let Tuple maybeTrack maybeInstrument =
          getSelectedTrackAndInstrument globalState

    in  case maybeTrack of
          Nothing -> []
          Just track ->
            case track of
              MasterTrack -> []
              LineNumberTrack -> []
              InstrumentTrack _ -> case maybeInstrument of
                Nothing -> []
                Just instrument ->
                  [ HH.slot
                      (Proxy :: _ "synth")
                      unit
                      Synth.component
                      { selectedInstrument: instrument
                      , socket: globalState.socket
                      }
                      absurd
                  ]
