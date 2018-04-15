module PointBuy exposing (..)

import Html exposing (Html)
import Html.Events
import Html.Attributes

buyInterface : tag -> number -> Html (tag,Int)
buyInterface = buyInterfaceWithDisable False

buyInterfaceWithDisable : Bool -> tag -> number -> Html (tag,Int)
buyInterfaceWithDisable disabled tag val =
    Html.input 
        [ Html.Attributes.type_ "number"
        , Html.Attributes.value <| toString val
        , Html.Attributes.disabled disabled
        , Html.Attributes.min "0"
        , Html.Attributes.size 1
        , Html.Events.onInput (String.toInt>>(Result.withDefault 0)>>((,) tag))
        , Html.Attributes.style 
            [("width","3em")]
        ] []