module Attributes exposing (..)

import Dict exposing (Dict)

import Priorities exposing (Priorities)

type Attribute 
    = BOD
    | AGI
    | REA
    | STR
    | WIL
    | LOG
    | INT
    | CHA
    | EDG
    | MAG
    | RES

type AttrGroup
    = BASE
    | SPECIAL

physical : List Attribute
physical = [BOD,AGI,REA,STR]

mental : List Attribute
mental = [WIL,LOG,INT,CHA]

base : List Attribute
base = physical++mental

special : List Attribute
special = [EDG,MAG,RES]

all : List Attribute
all = base++special

toAttribute : String -> Maybe Attribute
toAttribute str =
    case String.toUpper str of
        "BOD" -> Just BOD
        "BODY" -> Just BOD

        "AGI" -> Just AGI
        "AGILITY" -> Just AGI

        "REA" -> Just REA
        "REACTION" -> Just REA

        "STR" -> Just STR
        "STRENGTH" -> Just STR

        "WIL" -> Just WIL
        "WILL" -> Just WIL
        "WILLPOWER" -> Just WIL

        "LOG" -> Just LOG
        "LOGIC" -> Just LOG

        "INT" -> Just INT
        "INTUITION" -> Just INT

        "CHA" -> Just CHA
        "CHARISMA" -> Just CHA

        "EDG" -> Just EDG
        "EDGE" -> Just EDG

        "MAG" -> Just MAG
        "MAGIC" -> Just MAG

        "RES" -> Just RES
        "RESONANCE" -> Just RES

        _ -> Nothing

toPrettyString : Attribute -> String
toPrettyString attr =
    case attr of
        BOD -> "Body"
        AGI -> "Agility"
        REA -> "Reaction"
        STR -> "Strength"
        WIL -> "Willpower"
        LOG -> "Logic"
        INT -> "Intuition"
        CHA -> "Charisma"
        EDG -> "Edge"
        MAG -> "Magic"
        RES -> "Resonance"

type alias DictStore = Int

toDictStore : Attribute -> DictStore
toDictStore a =
    case a of
        BOD -> 0
        AGI -> 1
        REA -> 2
        STR -> 3
        WIL -> 4
        LOG -> 5
        INT -> 6
        CHA -> 7
        EDG -> 8
        MAG -> 9
        RES -> 10

fromDictStore : DictStore -> Maybe Attribute
fromDictStore i =
    case i of
        0 -> Just BOD
        1 -> Just AGI
        2 -> Just REA
        3 -> Just STR
        4 -> Just WIL
        5 -> Just LOG
        6 -> Just INT
        7 -> Just CHA
        8 -> Just EDG
        9 -> Just MAG
        10 -> Just RES
        _ -> Nothing

type alias AttrObj =
    Dict Int Int

attrObj : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> AttrObj
attrObj bod agi rea str will log int cha edg mag res =
    let
        values = 
            [bod,agi,rea,str,will,log,int,cha,edg,mag,res]
        tags = 
            List.map toDictStore all
    in
        values
            |> List.map2 (,) tags
            |> Dict.fromList


get : Attribute -> AttrObj -> Int
get attr attrobj =
    attrobj
        |> Dict.get (toDictStore attr)
        |> Maybe.withDefault 0

set : Attribute -> Int -> AttrObj -> AttrObj
set attr val =
    Dict.insert (toDictStore attr) val

increase : Attribute -> AttrObj -> AttrObj
increase attr attrobj =
    attrobj |> Dict.update 
        (toDictStore attr)
        (\j ->
            case j of 
                Nothing -> Nothing
                Just v -> Just (v+1)
        )

decrease : Attribute -> AttrObj -> AttrObj
decrease attr attrobj =
    attrobj |> Dict.update
        (toDictStore attr)
        (\j ->
            case j of
                Nothing -> Nothing
                Just v ->
                    if v>0 then
                        Just (v-1)
                    else
                        Just 0
        )

add : AttrObj -> AttrObj -> AttrObj
add lefts rights =
    let
        leftStep = (\_ _ r -> r)
        bothStep = (\tag left right -> Dict.insert tag (left+right))
        rightStep = (\_ _ r -> r)
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            lefts
            rights
            Dict.empty

type alias RuleCheck =
    { overMax : List Attribute
    , atMax : List Attribute
    , overSpent : Bool
    }


getPriorityPointCount : Priorities -> Int
getPriorityPointCount ps =
    case Priorities.getPriorityIndex Priorities.Attributes ps of
        0 -> 24
        1 -> 20
        2 -> 16
        3 -> 14
        _ -> 12


ruleCheck : AttrObj -> AttrObj -> AttrObj -> Priorities -> RuleCheck
ruleCheck bases boughts maximums ps =
    let
        leftStep = (\_ _ r -> r)
        bothStep = 
            (\sattr curr max r ->
                { r 
                    | overMax = if curr>max then sattr::r.overMax else r.overMax
                    , atMax = if curr==max then sattr::r.atMax else r.atMax
                    }
            )
        rightStep = (\_ _ r -> r)
        currents = add bases boughts
        overSpent = ((Dict.values>>List.sum) currents) > pointsAvailable 
        initialCheck = 
            Dict.merge
                leftStep
                bothStep
                rightStep
                currents
                maximums
                {overMax=[],atMax=[],overSpent=overSpent}
        pointsAvailable = getPriorityPointCount ps
        
    in
        { initialCheck 
            | overMax = List.filterMap fromDictStore initialCheck.overMax
            , atMax = List.filterMap fromDictStore initialCheck.overMax
            }

