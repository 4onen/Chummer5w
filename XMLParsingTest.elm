module XMLParsingTest exposing (main)

import Html exposing (Html)
import Html.Events

import List.Extra
import Xml.Decode

import C5WCharAttributes

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias ParseResult = (String,C5WCharAttributes.BaseAttributes)

type alias Model =
    { textboxContent : String
    , parseResult : Maybe (Result String ParseResult)
    }


type Msg
    = TextboxChanged String
    | ParseButtonClicked


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TextboxChanged s ->
            ({model | textboxContent = s}, Cmd.none)

        ParseButtonClicked ->
            ({model | parseResult = Just <| Xml.Decode.run parser model.textboxContent}, Cmd.none)


view : Model -> Html Msg
view model =
    Html.div [] 
        [ Html.textarea [Html.Events.onInput TextboxChanged] []
        , Html.button [Html.Events.onClick ParseButtonClicked] [Html.text "Parse"]
        , model.parseResult
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> Html.text
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    (Model "" Nothing, Cmd.none)


parser : Xml.Decode.Decoder ParseResult
parser =
    Xml.Decode.path ["metatypes","metatype"]
        <| Xml.Decode.list
        <| Xml.Decode.map2 
            (,)
            ( Xml.Decode.path ["name"] <| Xml.Decode.single Xml.Decode.string )
            ( Xml.Decode.
            )