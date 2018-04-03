module Attributes exposing (..)

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
    | MAGIC
    | RESONANCE

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
        Special MAGIC -> "MAG"
        Special RESONANCE -> "RES"

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
    , magic : Int
    , resonance : Int
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
            Special MAGIC -> .magic
            Special RESONANCE -> .resonance