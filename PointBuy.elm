module PointBuy exposing (..)

import Html exposing (Html)
import Html.Events
import Html.Attributes

type PointBuy a 
    = Buy a
    | Sell a
    | Set a Int

buyInterface : tag -> number -> List (Html (PointBuy tag))
buyInterface = buyInterfaceWithDisable False

buyInterfaceWithDisable : Bool -> tag -> number -> List (Html (PointBuy tag))
buyInterfaceWithDisable disabled tag val =
    [ Html.input 
        [ Html.Attributes.type_ "text" 
        , Html.Attributes.value <| toString val
        , Html.Attributes.size 1
        , Html.Attributes.disabled disabled
        , Html.Events.onInput (String.toInt>>(Result.withDefault 0)>>(Set tag))
        ] []
    , Html.button 
        [ Html.Events.onClick <| Buy tag
        , Html.Attributes.disabled disabled
        ]
        [ Html.text "+"]
    , Html.button 
        [ Html.Events.onClick <| Sell tag
        , Html.Attributes.disabled disabled
        ] 
        [ Html.text "-"]
    ]