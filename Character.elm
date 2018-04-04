module Character exposing (..)

import PointBy exposing (PointBy, pointBy)
import Metatype exposing (Metatype)
import Attributes exposing (AttributeObject)
import Priorities exposing (Priorities, PriorityChar(..))

type alias Character =
    { priorities : Priorities
    , race : Metatype 
    , attributePointBy : PointBy Attributes.BaseLabel
    , specialAttributePointBy : PointBy Attributes.SpecialLabel
    , mostRecentError : String
    }

default : Character
default = 
    Character
        Priorities.default
        (Metatype.Human Nothing)
        (pointBy (priorityToAttributeCount Priorities.default))
        (pointBy 3)
        ""


getAttributeObject : Character -> AttributeObject
getAttributeObject character =
    let
        magresPriority = Priorities.getChar Priorities.Talent character.priorities
        base = Metatype.getBaseStats character.race magresPriority
        spent = Attributes.pointBysToAttributeObject character.attributePointBy character.specialAttributePointBy
    in
        Attributes.add base spent

priorityToAttributeCount : Priorities -> Int
priorityToAttributeCount ps = 
    case Priorities.getChar Priorities.Attributes ps of
        APriority -> 24
        BPriority -> 20
        CPriority -> 16
        DPriority -> 14
        EPriority -> 12

spendPoint : Attributes.Label -> Character -> Character
spendPoint lbl character =
    case lbl of
        Attributes.Base pt ->
            let
                pointSpend = PointBy.spendPoint pt character.attributePointBy
            in
                case pointSpend of 
                    Result.Ok newPointBy ->
                        { character | attributePointBy = newPointBy }
                    Result.Err errStr ->
                        { character | mostRecentError = errStr }
        Attributes.Special pt ->
            let
                pointSpend = PointBy.spendPoint pt character.specialAttributePointBy
            in
                case pointSpend of 
                    Result.Ok newPointBy ->
                        { character | specialAttributePointBy = newPointBy }
                    Result.Err errStr ->
                        { character | mostRecentError = errStr }

unspendPoint : Attributes.Label -> Character -> Character
unspendPoint lbl character =
    case lbl of
        Attributes.Base pt ->
            let
                pointSpend = PointBy.unspendPoint pt character.attributePointBy
            in
                case pointSpend of 
                    Result.Ok newPointBy ->
                        { character | attributePointBy = newPointBy }
                    Result.Err errStr ->
                        { character | mostRecentError = errStr }
        Attributes.Special pt ->
            let
                pointSpend = PointBy.unspendPoint pt character.specialAttributePointBy
            in
                case pointSpend of 
                    Result.Ok newPointBy ->
                        { character | specialAttributePointBy = newPointBy }
                    Result.Err errStr ->
                        { character | mostRecentError = errStr }