module XMLParsingTest exposing (main)

import Html exposing (Html)
import Html.Events

import Dict

import Json.Encode

import Xml.Decode
import Xml.Decode.Pipeline exposing (requiredPath)

import C5WCharAttributes

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias ParseResults = List ParseResult

type alias ParseResult = 
    { name : String 
    , karmaCost : Int
    , source : String
    , sourcePage : Int
    , attributes : C5WCharAttributes.BaseAttributes
    }

type alias Model =
    { textboxContent : String
    , parseResult : Maybe (Result String ParseResults)
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
        , Html.div [] 
            [ model.parseResult
                |> Maybe.map toString
                |> Maybe.withDefault ""
                |> Html.text
            ]
        , Html.div [] 
            [ model.parseResult
                |> Maybe.map (Result.map myEncode)
                |> Maybe.map toString
                |> Maybe.withDefault ""
                |> Html.text
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    (Model "" Nothing, Cmd.none)


parser : Xml.Decode.Decoder ParseResults
parser =
    Xml.Decode.path ["metatypes","metatype"]
        <| Xml.Decode.list
        <| Xml.Decode.map2 identity 
            ( Xml.Decode.succeed ParseResult
                |> requiredPath ["name"] ( Xml.Decode.single Xml.Decode.string )
                |> requiredPath ["karma"] ( Xml.Decode.single Xml.Decode.int )
                |> requiredPath ["source"] ( Xml.Decode.single Xml.Decode.string )
                |> requiredPath ["page"] ( Xml.Decode.single Xml.Decode.int )
            )
            ( attributeDicter )
            
attributeDicter : Xml.Decode.Decoder C5WCharAttributes.BaseAttributes
attributeDicter =
    let
        attributeTupler attr =
            Xml.Decode.map2
                (,)
                ( Xml.Decode.path [attr++"min"] <| Xml.Decode.single Xml.Decode.int)
                ( Xml.Decode.path [attr++"max"] <| Xml.Decode.single Xml.Decode.int)
        baseAttributeNameToDecoder attr = 
            Xml.Decode.map
                ((,) attr)
                (attributeTupler attr)
        baseAttributeDecoderFolder incoming addTo =
            Xml.Decode.map2
                (::)
                incoming
                addTo
        baseAttributesDecoder = 
            C5WCharAttributes.baseAttributesNames
                |> List.map baseAttributeNameToDecoder
                |> List.foldl baseAttributeDecoderFolder (Xml.Decode.succeed [])
    in
        Xml.Decode.map Dict.fromList baseAttributesDecoder


myEncode : ParseResults -> Json.Encode.Value
myEncode parseResults =
    let
        encodeAttributes : C5WCharAttributes.BaseAttributes -> Json.Encode.Value
        encodeAttributes a = 
            Dict.toList a
                |> List.map 
                    (\t -> 
                        Json.Encode.object 
                            [ ( "attrName",Json.Encode.string <| Tuple.first t )
                            , ( "min", Json.Encode.int <| Tuple.first <| Tuple.second t )
                            , ( "mnax",Json.Encode.int <| Tuple.second <| Tuple.second t )
                            ]
                    )
                |> Json.Encode.list
        encodeResult r =
            Json.Encode.object
                [ ("name", Json.Encode.string r.name )
                , ("karmaCost", Json.Encode.int r.karmaCost )
                , ("source", Json.Encode.string r.source )
                , ("page", Json.Encode.int r.sourcePage )
                , ("attributes", encodeAttributes r.attributes )
                ]
    in
        parseResults
            |> List.map encodeResult  
            |> Json.Encode.list