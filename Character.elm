module Character exposing (..)

import Html exposing (Html)
import Html.Events
import Html.Attributes

import HtmlInputs

import Priorities exposing (Priorities)
import Magicality exposing (Magicality(..))

type alias Character =
    { name : String
    , ignoreCharacterCreationRules : Bool
    , priorities : Priorities
    , prioritiesLocked : Bool
    , magicClass : Magicality
    }

type Msg
    = PrioritiesMsg Priorities.Msg
    | ChangeMagicality Magicality
    | ToggleCharacterCreationRules
    | TogglePriorityLock

default : Character
default =
    Character
        ""
        False
        Priorities.default
        False
        Magicality.Magician

update : Msg -> Character -> Character
update msg model =
    case msg of
        ToggleCharacterCreationRules ->
            {model|ignoreCharacterCreationRules = not model.ignoreCharacterCreationRules}
        PrioritiesMsg m ->
            {model|priorities = Priorities.update m model.priorities}
        ChangeMagicality new ->
            {model|magicClass = new}
        TogglePriorityLock ->
            {model|prioritiesLocked = not model.prioritiesLocked}

view : Character -> Html Msg
view model =
    Html.div [] 
        [ HtmlInputs.viewCheckbox "Ignore character creation rules" ToggleCharacterCreationRules model.ignoreCharacterCreationRules
        , viewPriorities model
        , viewMagicalityList model
        ]

viewPriorities : Character -> Html Msg
viewPriorities model =
    Html.fieldset []
        [ Priorities.view (not model.prioritiesLocked) model.magicClass PrioritiesMsg model.priorities
        , HtmlInputs.viewCheckbox "Priority rearrangement" TogglePriorityLock (not model.prioritiesLocked)
        ]


viewMagicalityList : Character -> Html Msg
viewMagicalityList model =
    let
        magPriorityIdx = Priorities.getPriorityIndex Priorities.MagicOrResonance model.priorities
        magList = if magPriorityIdx==4 then [Magicality.Adept] else Magicality.listOfTypes
        search tgt ps =
            case ps of
                p::rest ->
                    if p==tgt then 0 else (1+(search tgt rest))
                [] ->
                    99
        selected = if magPriorityIdx==4 then 0 else search model.magicClass magList
        labelList = List.map ((flip Magicality.viewMagicClass) magPriorityIdx) magList 
        msgList = List.map ChangeMagicality magList
    in
        HtmlInputs.viewRadioButtons labelList msgList selected

subscriptions : Character -> Sub Msg
subscriptions model =
    Sub.map PrioritiesMsg <| Priorities.subscriptions model.priorities
