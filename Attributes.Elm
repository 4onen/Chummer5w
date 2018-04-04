module Attributes exposing (..)

import PointBy exposing (PointBy)

type Label 
    = Base BaseLabel
    | Special SpecialLabel

type BaseLabel
    = BODY
    | AGILITY
    | REACTION
    | STRENGTH
    | CHARISMA
    | INTUITION
    | LOGIC
    | WILLPOWER

type SpecialLabel 
    = EDGE
    | MAGRES

baseLabelList : List BaseLabel
baseLabelList =
    [ BODY, AGILITY, REACTION
    , STRENGTH, CHARISMA
    , INTUITION, LOGIC
    , WILLPOWER
    ]

specialLabelList : List SpecialLabel
specialLabelList =
    [ EDGE, MAGRES ]

labelList : List Label
labelList = 
    let
        taggedBases = List.map Base baseLabelList
        taggedSpecials = List.map Special specialLabelList
    in
        taggedBases ++ taggedSpecials

toString : Label -> String
toString lbl =
    case lbl of
        Base BODY -> "BOD"
        Base AGILITY -> "AGI"
        Base REACTION -> "REA"
        Base STRENGTH -> "STR"
        Base CHARISMA -> "CHA"
        Base INTUITION -> "INT"
        Base LOGIC -> "LOG"
        Base WILLPOWER -> "WIL"
        Special EDGE -> "EDGE"
        Special MAGRES -> "MAG/RES"

type alias AttributeObject =
    { body : Int
    , agility : Int
    , reaction : Int
    , strength : Int
    , charisma : Int
    , intuition : Int
    , logic : Int
    , willpower : Int
    , edge : Int
    , magres : Int
    }

getAttribute : Label -> AttributeObject -> Int
getAttribute class attobj =
    attobj
        |> case class of
            Base BODY -> .body
            Base AGILITY -> .agility
            Base REACTION -> .reaction
            Base STRENGTH -> .strength
            Base CHARISMA -> .charisma
            Base INTUITION -> .intuition
            Base LOGIC -> .logic
            Base WILLPOWER -> .willpower
            Special EDGE -> .edge
            Special MAGRES -> .magres

baseGetterList : List (AttributeObject -> Int)
baseGetterList =
    [ .body,.agility,.reaction,.strength,.charisma
    , .intuition,.logic,.logic,.willpower ]

specialGetterList : List (AttributeObject -> Int)
specialGetterList =
    [ .edge,.magres ]

getterList : List (AttributeObject -> Int)
getterList = baseGetterList ++ specialGetterList

addBases : AttributeObject -> AttributeObject -> AttributeObject
addBases a1 {body,agility,reaction,strength,charisma,intuition,logic,willpower} =
    { a1 | body = a1.body+body, agility = a1.agility+agility
    , reaction = a1.reaction + reaction, strength = a1.strength + strength
    , charisma = a1.charisma + charisma, intuition = a1.intuition + intuition
    , logic = a1.logic + logic, willpower = a1.willpower + willpower
    }

addSpecials : AttributeObject -> AttributeObject -> AttributeObject
addSpecials a1 {edge,magres} =
    { a1 | edge = a1.edge + edge, magres = a1.magres + magres }

add : AttributeObject -> AttributeObject -> AttributeObject
add a1 a2 =
    a1
        |> flip addBases a2
        |> flip addSpecials a2

pointBysToAttributeObject : PointBy BaseLabel -> PointBy SpecialLabel -> AttributeObject
pointBysToAttributeObject bases specials =
    { body = PointBy.countPoints BODY bases
    , agility = PointBy.countPoints AGILITY bases
    , reaction = PointBy.countPoints REACTION bases
    , strength = PointBy.countPoints STRENGTH bases
    , charisma = PointBy.countPoints CHARISMA bases
    , intuition = PointBy.countPoints INTUITION bases
    , logic = PointBy.countPoints LOGIC bases
    , willpower = PointBy.countPoints WILLPOWER bases
    , edge = PointBy.countPoints EDGE specials
    , magres = PointBy.countPoints MAGRES specials
    }