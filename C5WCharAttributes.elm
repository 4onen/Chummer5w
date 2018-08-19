module C5WCharAttributes exposing (..)

import Dict exposing (Dict)

type alias Attributes =
    { base : BaseAttributes
    , special : SpecAttributes
    , baseKarma : BaseAttributes
    , specialKarma : SpecAttributes
    }

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
    , magres : Int
    --}

{--calcKarmaUse : Attributes -> Int
calcKarmaUse attrs =
    let
        baseKarmaUse =
            attrs.base
                |> 
    in--}
        