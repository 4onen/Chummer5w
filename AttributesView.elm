module AttributesView exposing (view)

import Html exposing (Html)
import Html.Attributes

import Dict exposing (Dict)
import List.Extra

import PointBuy
import Attributes exposing (..)

view : AttrObj -> AttrObj -> AttrObj -> AttrObj-> Int -> Int -> ((Attribute,Int) -> msg) -> ((Attribute,Int) -> msg) -> Html msg
view bases bought karmas maxes availableAttributePoints availableSpecialPoints pointTagger karmaPointTagger =
    Html.details []
        [ Html.summary [] [Html.text "Attributes"]
        , Html.table []
            [ Html.thead [] 
                [ Html.th [] []
                , Html.th [] [Html.text "Attribute"]
                , Html.th [] []
                , Html.th [] []
                , Html.th [] []
                , Html.th [] [Html.text "Points"]
                , Html.th [] [Html.text "Karma"]
                ]
            , viewtbody bases bought karmas maxes availableAttributePoints availableSpecialPoints pointTagger karmaPointTagger
            ]
        ]


viewtbody : AttrObj -> AttrObj -> AttrObj -> AttrObj-> Int -> Int -> ((Attribute,Int) -> msg) -> ((Attribute,Int) -> msg) -> Html msg
viewtbody bases bought karmas maxes availableAttributePoints availableSpecialPoints pointTagger karmaPointTagger =
    ( List.map4 
        (\(a1,c) (a2, s) (a3,k) (a4,m) ->
            if a1==a2 && a2 == a3 && a3==a4 then
                case fromDictStore a1 of
                    Just a ->
                        Just (a,c,s,k,m)
                    Nothing ->
                        Nothing
            else 
                Nothing
        ) (Dict.toList bases) (Dict.toList bought) (Dict.toList karmas) (Dict.toList maxes) )
        |> List.filterMap identity
        |> List.map (\(a,c,spent,k,m) -> (viewAttribute (a,c,spent,k,m) pointTagger karmaPointTagger,spent))
        |> List.Extra.splitAt 8
        |> (\(simples,specials) -> 
            let
                simpleViews =
                    simples
                        |> List.map Tuple.first
                specialViews = 
                    specials
                        |> List.map Tuple.first
                simpleSpent = 
                    simples
                        |> List.map Tuple.second
                        |> List.sum
                specialSpent =
                    specials
                        |> List.map Tuple.second
                        |> List.sum
                simplePointDisplay =
                    Html.tr [] 
                        ((List.repeat 5 (Html.td [] []))
                        ++[Html.td [] [Html.text <| Basics.toString (availableAttributePoints - simpleSpent) ++ " Points"]]
                        )
                specialPointDisplay =
                    Html.tr []
                        ((List.repeat 5 (Html.td [] []))
                        ++[Html.td [] [Html.text <| Basics.toString (availableSpecialPoints - specialSpent) ++ " Special"]]
                        )
            in
                simpleViews++(simplePointDisplay::specialViews++[specialPointDisplay])
            )
        |> Html.tbody []


viewAttribute : (Attribute, Int, Int, Int, Int) -> ((Attribute,Int) -> msg) -> ((Attribute,Int) -> msg) -> Html msg
viewAttribute (attr,base,bought,karmaPoints,maxVal) pointTagger karmaPointTagger =
    let
        current = if maxVal>0 then base+bought+karmaPoints else 0
        calcKarma k =
            if k<1 then 
                0
            else
                ((base+bought+k)*5+(calcKarma (k-1)))
    in
        Html.tr []
            [ Html.td 
                [Html.Attributes.style [("text-align","center")]] 
                [attr |> Basics.toString |> Html.text]
            , Html.td [] [attr |> Attributes.toPrettyString |> Html.text]
            , Html.td [] [current |> Basics.toString |> Html.text]
            , Html.td [] [Html.text "/"]
            , Html.td [] [maxVal |> Basics.toString |> Html.text]
            , Html.map pointTagger 
                <| Html.td [] 
                    [PointBuy.buyInterfaceWithDisable (maxVal<1) attr (if maxVal>0 then bought else 0)]
            , Html.map karmaPointTagger 
                <| Html.td []
                    [PointBuy.buyInterfaceWithDisable (maxVal<1) attr (if maxVal>0 then karmaPoints else 0)]
            , Html.td [] 
                [ Html.text <|
                    if karmaPoints < 1 then 
                        ""
                    else 
                        (toString <| calcKarma karmaPoints)++" Karma"
                ]
            ]
