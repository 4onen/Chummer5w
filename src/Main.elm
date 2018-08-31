module Main exposing (main)

import Browser
import Html
import Html.Events
import Http
import Parser exposing (Parser,(|.),(|=))

import Qualities

main = 
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

type alias Model = 
    { fileContent : String
    , parseResult : Maybe (Result (List Parser.DeadEnd) (List String))
    , show : Bool
    }


type Msg 
    = FileLoaded (Result Http.Error String)
    | Show

init () =
    ( Model "init" Nothing False
    , Http.send FileLoaded <| Http.getString "https://raw.githubusercontent.com/chummer5a/chummer5a/master/Chummer/data/critters.xml"
    )

update msg model =
    case msg of
        FileLoaded (Result.Ok contentStr) ->
            (
                { model 
                | fileContent = contentStr
                , parseResult = Just <| Parser.run Qualities.fileParser contentStr
                }
            , Cmd.none
            )
        FileLoaded (Result.Err e) ->
            ({model | fileContent = Debug.toString e}, Cmd.none)
        Show ->
            ({model | show = not model.show}, Cmd.none)

view model = 
    { title = "C5W"
    , body = 
        case model.parseResult of
            Nothing ->
                [ Html.text model.fileContent ]
            Just (Result.Err errs) ->
                let
                    lines = String.lines model.fileContent
                in
                    errs
                        |> List.map 
                            (\err -> 
                                lines
                                    |> List.drop (err.row-1)
                                    |> List.take 1
                                    |> List.map (\l -> Html.p [] [Html.text l])
                                    |> (::) (Html.p [] [Html.text <| Debug.toString err])
                            )
                        |> List.map (Html.div [])
            Just (Result.Ok q) ->
                (Html.button [Html.Events.onClick Show] [Html.text "Show"]) ::
                case model.show of
                    False ->
                        List.singleton <| Html.text "done" 
                    True ->
                        q |> List.map Html.text
    }

