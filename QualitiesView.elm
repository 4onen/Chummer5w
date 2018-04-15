module QualitiesView exposing (..)

import Html exposing (Html)
import Html.Events
import Html.Attributes

import Modifiers exposing (Modifier)
import Qualities exposing (..)


type alias AddQualityModel =
    { name : String 
    , warnNameLength : Bool
    , karmaCost : Int
    , qualityTooltip : String
    , qualityModifier : Modifier QualityTarget
    }

initAddQuality : AddQualityModel
initAddQuality =
    AddQualityModel
        ""
        False
        0
        ""
        (Modifiers.Plus 0 (Attribute "BOD"))

viewAddQuality : AddQualityModel -> Html AddQualityModel
viewAddQuality model =
    Html.fieldset []
        [ Html.input 
            [ Html.Attributes.type_ "text"
            , Html.Attributes.placeholder "Quality name"
            , Html.Attributes.value model.name
            , Html.Events.onInput (\s -> {model|name=s})
            ] []
        , Html.input 
            [ Html.Attributes.type_ "number"
            , Html.Attributes.value (Basics.toString model.karmaCost)
            , Html.Events.onInput (\s -> {model|karmaCost = Result.withDefault 0 <| String.toInt s})
            ] []
        ]