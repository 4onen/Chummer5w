module Attributes exposing (..)

import Dict exposing (Dict)

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

type alias AttrObj =
    Dict String Int

attrObj : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> AttrObj
attrObj bod agi rea str will log int cha edg mag res =
    [bod,agi,rea,str,will,log,int,cha,edg,mag,res]
        |> List.map2 (,) all
        |> List.map (Tuple.mapFirst Basics.toString)
        |> Dict.fromList


get : Attribute -> AttrObj -> Int
get attr attrobj =
    attrobj
        |> Dict.get (Basics.toString attr)
        |> Maybe.withDefault 0

increase : Attribute -> AttrObj -> AttrObj
increase attr attrobj =
    attrobj |> Dict.update 
        (Basics.toString attr)
        (\j ->
            case j of 
                Nothing -> Nothing
                Just v -> Just (v+1)
        )

decrease : Attribute -> AttrObj -> AttrObj
decrease attr attrobj =
    attrobj |> Dict.update
        (Basics.toString attr)
        (\j ->
            case j of
                Nothing -> Nothing
                Just v ->
                    if v>0 then
                        Just (v-1)
                    else
                        Just 0
        )