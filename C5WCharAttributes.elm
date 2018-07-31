module C5WCharAttributes exposing (..)

type alias Attributes =
    { base : BaseAttributes
    , special : SpecAttributes
    , baseKarma : BaseAttributes
    , specialKarma : SpecAttributes
    }

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

calcKarmaUse : Attributes -> Int
calcKarmaUse attrs =
    let
        baseKarmaUse =
            attrs.base
                |> 
    in
        