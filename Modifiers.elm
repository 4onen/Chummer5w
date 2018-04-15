module Modifiers exposing (..)

type Modifier target
    = Plus Int target
    | Minus Int target
    | Set Int target
    | Mult Int target
    | DivRndUp Int target
    | DivRndDwn Int target
    | DivRndNml Int target

type ModifierTarget
    = Attribute String
    | Skill String
    | SkillGroup String
    | KnowledgeSkill String
    | KnowledgeSkillGroup String

getTarget : Modifier target -> target
getTarget m =
    case m of
        Plus _ tgt -> tgt
        Minus _ tgt -> tgt
        Set _ tgt -> tgt
        Mult _ tgt -> tgt
        DivRndUp _ tgt -> tgt
        DivRndDwn _ tgt -> tgt
        DivRndNml _ tgt -> tgt

apply : Modifier target -> Int -> Int
apply m i =
    case m of
        Plus j _ ->
            i+j
        Minus j _ ->
            i-j
        Set j _ ->
            j
        Mult j _ ->
            i*j
        DivRndUp j _ ->
            ceiling ((toFloat i)/(toFloat j))
        DivRndDwn j _ ->
            floor ((toFloat i)/(toFloat j))
        DivRndNml j _ ->
            round ((toFloat i)/(toFloat j))

