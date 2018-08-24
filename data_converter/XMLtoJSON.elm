module XMLtoJSON exposing (main)

import Html exposing (Html)
import Html.Events

import Http

import Json.Encode
import Json.Decode

import Xml.Decode
import Xml.Decode.Pipeline exposing (requiredPath)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type ResourceStatus
    = Prefetch
    | Fetching
    | Fetched (Result Http.Error String)

type alias Model =
    { resourceName : String
    , resourceData : ResourceStatus
    }

type Msg
    = TextChanged String
    | ImportButtonClicked
    | LoadData (Result Http.Error String)

init : (Model, Cmd Msg)
init = Model "" Prefetch ! []

subscriptions : Model -> Sub Msg
subscriptions = always Sub.none

view : Model -> Html Msg
view model =
    Html.div [] 
        [ Html.div []
            [ Html.input [ Html.Events.onInput TextChanged ] [ ]
            , Html.button [ Html.Events.onClick ImportButtonClicked ] [Html.text "Fetch"]
            ]
        , Html.text <| 
            case model.resourceData of
                Prefetch ->
                    ""
                Fetching ->
                    "Loading from the 'net..."
                Fetched dat ->
                    case dat of
                        Result.Err err ->
                            toString err
                        Result.Ok str ->
                            str
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TextChanged str ->
            { model | resourceName = str } ! []
        ImportButtonClicked ->
            let
                str = model.resourceName
                tgt = if String.endsWith ".xml" <| String.toLower str then str else str++".xml"
            in
                ({ model | resourceData = Fetching }, Http.send LoadData <| Http.getString ("/../data/"++tgt))
        LoadData result -> 
            { model | resourceData = Fetched result } ! []