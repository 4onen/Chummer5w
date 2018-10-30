module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events
import Http
import Modifier exposing (Modifier)
import Xml.Decode as XD


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { fileContent : String
    , parseResult : Maybe (Result String (List Modifier))
    , show : Bool
    }


type Msg
    = FileLoaded (Result Http.Error String)
    | Show


init : () -> ( Model, Cmd Msg )
init () =
    ( Model "init" Nothing False
    , Http.send FileLoaded <| Http.getString "https://raw.githubusercontent.com/chummer5a/chummer5a/master/Chummer/data/qualities.xml"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileLoaded (Result.Ok contentStr) ->
            ( { model
                | fileContent = contentStr
                , parseResult = Just <| XD.run parser contentStr
              }
            , Cmd.none
            )

        FileLoaded (Result.Err e) ->
            ( { model | fileContent = Debug.toString e }, Cmd.none )

        Show ->
            ( { model | show = not model.show }, Cmd.none )


parser : XD.Decoder (List Modifier)
parser =
    XD.path [ "qualities", "quality" ] (XD.list Modifier.qualityModifier)


view : Model -> Browser.Document Msg
view model =
    { title = "C5W"
    , body =
        case model.parseResult of
            Nothing ->
                [ Html.text model.fileContent ]

            Just (Result.Err err) ->
                [ Html.text err ]

            Just (Result.Ok qs) ->
                Html.button [ Html.Events.onClick Show ] [ Html.text "Show" ]
                    :: (case model.show of
                            False ->
                                [ Html.text "done" ]

                            True ->
                                qs
                                    |> List.map viewModifier
                                    |> Html.table []
                                    |> List.singleton
                       )
    }


viewModifier : Modifier.Modifier -> Html Msg
viewModifier q =
    Html.tr []
        [ Html.td [] [ Html.text q.id ]
        , Html.td []
            [ Html.text
                (case q.effects.effectCategory of
                    Modifier.Positive ->
                        "Positive"

                    Modifier.Negative ->
                        "Negative"

                    Modifier.Undefined ->
                        "Undefined"
                )
            ]
        , Html.td [] [ Html.text q.name ]
        , Html.td [] [ Html.text q.source.sourceBook ]
        , Html.td [] [ Html.text <| String.fromInt q.source.sourcePage ]
        ]
