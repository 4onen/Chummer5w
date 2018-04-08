module AttributesView exposing (view)

import Html exposing (Html)
import Html.Events
import Html.Attributes

import Dict exposing (Dict)

import PointBuy exposing (PointBuy(..))
import Attributes exposing (..)

view : AttrObj -> AttrObj -> AttrObj -> Int -> Html (PointBuy Attribute)
view bases bought maxes availableAttributePoints =
    List.map3 
        (\(a1,c) (a2, s) (a3,m) ->
            if a1==a2 && a2 == a3 then
                case fromDictStore a1 of
                    Just a ->
                        Just (a,c,s,m)
                    Nothing ->
                        Nothing
            else 
                Nothing
        ) (Dict.toList bases) (Dict.toList bought) (Dict.toList maxes)
        |> List.filterMap identity
        |> List.map viewAttribute
        |> ((flip (++))
                [ Html.tr [] 
                    ((List.repeat 5 <| Html.td [] [])
                    ++[Html.td [] 
                        [ bought
                            |> Dict.values
                            |> List.sum
                            |> (-) availableAttributePoints
                            |> Basics.toString
                            |> (flip (++)) " points"
                            |> Html.text
                        ]])
                ]
            ) 
        |> Html.table []
        |> List.singleton
        |> (::) (Html.summary [] [Html.text "Attributes"])
        |> Html.details []

viewAttribute : (Attribute, Int, Int, Int) -> Html (PointBuy Attribute)
viewAttribute (attr,base,bought,maxVal) =
    let
        current = if maxVal>0 then base+bought else 0
    in
        Html.tr []
            [ Html.td 
                [Html.Attributes.style [("text-align","center")]] 
                [attr |> Basics.toString |> Html.text]
            , Html.td [] [attr |> Attributes.toPrettyString |> Html.text]
            , Html.td [] [current |> Basics.toString |> Html.text]
            , Html.td [] [Html.text "/"]
            , Html.td [] [maxVal |> Basics.toString |> Html.text]
            , Html.td [] 
                (PointBuy.buyInterfaceWithDisable (maxVal<1) attr (if maxVal>0 then bought else 0))
            ]