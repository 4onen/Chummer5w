module Priorities exposing (..)

import Html exposing (Html)

import ReorderableList

import Magicality exposing (Magicality(..))

type Priority
    = Metatype
    | Attributes
    | MagicOrResonance
    | Skills
    | Resources

type alias Priorities =
    ReorderableList.Model Priority

type alias Msg =
    ReorderableList.Msg


default : Priorities
default = 
    ReorderableList.fromList
        [ Metatype
        , Attributes
        , MagicOrResonance
        , Skills
        , Resources
        ]

init : List Priority -> Result String Priorities
init lst =
    case lst of
        [pa,pb,pc,pd,pe] ->
            if (pa/=pb) && (pa/=pc) && 
                (pa/=pd) && (pa/=pe) && 
                (pb/=pc) && (pb/=pd) && 
                (pb/=pe) && (pc/=pd) && 
                (pc/=pe) && (pd/=pe) then
                    Result.Ok (ReorderableList.fromList lst)
            else
                Result.Err "Non-unique priority entry in list."
        _::_::_::_::_::rest ->
            Result.Err ("List too large to be priority list: "
                ++toString (List.length rest)++" elements too large.")
        _ ->
            Result.Err ("List too small to be priority list: "
                ++toString (5-List.length lst)++" elements too small.")

update : ReorderableList.Msg -> Priorities -> Priorities
update msg model =
    ReorderableList.update msg model

subscriptions : Priorities -> Sub ReorderableList.Msg
subscriptions model =
    ReorderableList.subscriptions model

view : Bool -> Magicality -> (ReorderableList.Msg -> msg) -> Priorities -> Html msg
view enable mag msg model =
    ReorderableList.viewWithOptions
        msg
        ( ReorderableList.Options
            enable
            (Just viewRowTag)
            (Just <| viewPriority mag)
            (Just viewRaiseLowerButton)
        )
        model

viewRowTag : Int -> Html msg
viewRowTag idx =
    ( case idx of
        0 -> "A"
        1 -> "B"
        2 -> "C"
        3 -> "D"
        4 -> "E"
        _ -> "ERR"
    ) |> Html.text

viewRaiseLowerButton : Int -> a -> Bool -> Html msg
viewRaiseLowerButton _ _ up =
    if up then
        Html.text "^"
    else
        Html.text "v"

viewPriority : Magicality -> Int -> Priority -> Html msg
viewPriority mag idx p =
    ( case (p,idx) of
        (Metatype,0) ->
            "Metatype -- All Types"
        (Metatype,1) ->
            "Metatype -- All Types"
        (Metatype,2) ->
            "Metatype -- Human,Elf,Dwarf,Ork"
        (Metatype,3) ->
            "Metatype -- Human,Elf"
        (Metatype,4) ->
            "Metatype -- Human only"
        (Attributes,0) ->
            "Attributes -- 24"
        (Attributes,1) ->
            "Attributes -- 20"
        (Attributes,2) ->
            "Attributes -- 16"
        (Attributes,3) ->
            "Attributes -- 14"
        (Attributes,4) ->
            "Attributes -- 12"
        (MagicOrResonance,i) ->
            Magicality.viewMagicRating mag i
        (Skills,0) ->
            "Skills -- 46/10 Skills/Groups"
        (Skills,1) ->
            "Skills -- 36/5 Skills/Groups"
        (Skills,2) ->
            "Skills -- 28/2 Skills/Groups"
        (Skills,3) ->
            "Skills -- 22 Skills Only"
        (Skills,4) ->
            "Skills -- 18 Skills Only"
        (Resources,0) ->
            "Resources -- 450,000¥"
        (Resources,1) ->
            "Resources -- 275,000¥"
        (Resources,2) ->
            "Resources -- 140,000¥"
        (Resources,3) ->
            "Resources -- 50,000¥"
        (Resources,4) ->
            "Resources -- 6,000¥"
        _ -> toString p
    ) |> Html.text

getPriorityIndex : Priority -> Priorities -> Int
getPriorityIndex p ps =
    let
        search p ps =
            case ps of
                elem::rest ->
                    if elem==p then
                        0
                    else
                        1+(search p rest)
                [] ->
                    10
    in
        search p ps.data