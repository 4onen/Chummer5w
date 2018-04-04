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
    }

default : Character
default = 
    Character
        Priorities.default
        (Metatype.Human Nothing)
        (pointBy (priorityToAttributeCount Priorities.default))
        (pointBy 3)


getAttributeObject : Character -> AttributeObject
getAttributeObject character =
    let
        magresPriority = Priorities.getChar Priorities.Talent character.priorities
        base = Metatype.getBaseStats character.race magresPriority
    in
        base

priorityToAttributeCount : Priorities -> Int
priorityToAttributeCount ps = 
    case Priorities.getChar Priorities.Attributes ps of
        APriority -> 24
        BPriority -> 20
        CPriority -> 16
        DPriority -> 14
        EPriority -> 12