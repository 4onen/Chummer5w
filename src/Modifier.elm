module Modifier exposing (Modifier,CanPurchase(..))

import Dict exposing (Dict)
import Xml.Decode as XD

type CanPurchase
    = CharGenOnly
    | CampaignOnly
    | Always


type alias Modifier =
    { id : String
    , name : String
    , karmaChange : Int
    , canPurchase : CanPurchase
    , limitTimesTaken : Int
    , chargenEffects : Dict String Int
    , mustHaves : List String
    , mustNotHaves : List String
    , effects : Dict String Int
    , sourceBook : String
    , sourcePage : Int
    , nameOnPage : Maybe String
    }

--TODO: qualityToModifier test function