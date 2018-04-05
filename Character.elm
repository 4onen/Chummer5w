module Character exposing (..)

import Html exposing (Html)
import Html.Events

import Priorities exposing (Priorities)
import Magicality exposing (Magicality(..))

type alias Character =
    { priorities : Priorities
    , prioritiesLocked : Bool
    , magicClass : Magicality
    }

type Msg
    = PrioritiesMsg Priorities.Msg
    | TogglePriorityLock

default : Character
default =
    Character
        Priorities.default
        False
        Magicality.Magician

update : Msg -> Character -> Character
update msg model =
    case msg of
        PrioritiesMsg m ->
            {model|priorities = Priorities.update m model.priorities}
        TogglePriorityLock ->
            {model|prioritiesLocked = not model.prioritiesLocked}

view : Character -> Html Msg
view model =
    Html.div [] 
        [ Priorities.view (not model.prioritiesLocked) model.magicClass PrioritiesMsg model.priorities
        , Html.text <| Magicality.viewMagicClass model.magicClass (Priorities.getPriorityIndex Priorities.MagicOrResonance model.priorities)
        , Html.button 
            [Html.Events.onClick TogglePriorityLock] 
            [Html.text <| if model.prioritiesLocked then "Lock Priorities" else "Unlock Priorities"]
        ]