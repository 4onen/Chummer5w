module TestMain exposing (main)

import Html exposing (Html)
import Html.Events
import Html.Attributes

import Character exposing (Character)

main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
    { character : Character }

type Msg =
    CharacterMsg Character.Msg

init = 
    (Model Character.default) ! []

update msg model =
    case msg of
        CharacterMsg m ->
            {model|character = Character.update m model.character} ! []

view model =
    Html.map CharacterMsg <| (Character.viewSelector>>Character.view) model.character

subscriptions model =
    Sub.map CharacterMsg <| Character.subscriptions model.character
