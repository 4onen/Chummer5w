module C5WRaces exposing (..)

import Dict exposing (Dict)

type alias Race =
    { name : String 
    , karmaCost : Int
    , source : String
    , sourcePage : Int
    , baseAttributes : BaseAttributes
    , specAttributes : SpecAttributes
    }

jsonNames = ["metatypes.xml"]

xmlNames = ["metatypes.xml"]

baseAttributesNames = ["bod","agi","rea","str","cha","int","log","wil"]

specAttributesNames = ["edg","mag","res"]

type alias BaseAttributes = Dict String (Int,Int)
    {-- bod : Int
    , agi : Int
    , rea : Int
    , str : Int
    , wil : Int
    , log : Int
    , int : Int
    , cha : Int
    --}

type alias SpecAttributes = Dict String (Int,Int)
    {-- edg : Int
    , ess : Float
    , mag : Int
    , res : Int
    --}

