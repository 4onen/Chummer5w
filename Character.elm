module Character exposing (..)

import Html exposing (Html)
import Html.Events
import Html.Attributes

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
        TogglePriorityLock ->
            {model|prioritiesLocked = not model.prioritiesLocked}

view : Character -> Html Msg
view model =
    Html.div [] 
        [ viewCheckbox "Ignore character creation rules" ToggleCharacterCreationRules model.ignoreCharacterCreationRules
        , Priorities.view (not model.prioritiesLocked) model.magicClass PrioritiesMsg model.priorities
        , Html.text <| Magicality.viewMagicClass model.magicClass (Priorities.getPriorityIndex Priorities.MagicOrResonance model.priorities)
        , viewCheckbox "Priority rearrangement" TogglePriorityLock (not model.prioritiesLocked)
        ]

subscriptions : Character -> Sub Msg
subscriptions model =
    Sub.map PrioritiesMsg <| Priorities.subscriptions model.priorities


viewCheckbox : String -> Msg -> Bool -> Html Msg
viewCheckbox label msg checked =
    Html.label []
        [ Html.input 
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked checked
            , Html.Events.onClick msg] []
        , Html.text label
        ]
