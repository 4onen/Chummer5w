module Character exposing (..)

import PointBy exposing (PointBy)
import Metatype exposing (Metatype)
import Attributes
import Priorities exposing (Priorities)

type alias Character =
    { priorities : Priorities
    , race : Metatype 
    , attributePointBy : PointBy Attributes.BaseLabel
    , specialAttributePointBy : PointBy Attributes.SpecialLabel
    }

makeCharacter : Int -> Int -> Metatype -> Character
makeCharacter attributeCount specialAttributeCount race =
    Character
        ( Priorities.default )
        ( race )
        ( PointBy.pointBy attributeCount )
        ( PointBy.pointBy specialAttributeCount )

default : Character
default = makeCharacter 20 3 (Metatype.Human Nothing)