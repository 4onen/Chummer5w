module Modifier exposing (Modifier,CanPurchase(..))

import Dict exposing (Dict)
import Set exposing (Set)
import Xml.Decode as XD exposing (XmlContentDecoder)

type CanPurchase
    = CharGenOnly
    | CampaignOnly
    | Always

type alias Sourcing =
    { sourceBook : String
    , sourcePage : Int
    , nameOnPage : Maybe String
    }

type alias ModifierName = String

type alias Id = String

type alias Modifier =
    { id : Id
    , name : ModifierName
    , effects : Effects
    , requirements : Requirements
    , source : Sourcing
    }

qualityModifier : XmlContentDecoder Modifier
qualityModifier =
    XD.map6 Modifier
        <| XD.tag "id" XD.string
        <| XD.tag "name" XD.string
        <| qualityEffects
        <| qualityRequirements
        <| sourceIt

type alias Effects =
    { karmaChange : Int
    , chargenEffets : Dict String Int
    , alwaysEffects : Dict String Int
    }

qualityEffects : XmlContentDecoder Effects
qualityEffects =
    XD.succeed <| Effects 0 Dict.empty Dict.empty

type alias Requirements = 
    { canPurchase : CanPurchase
    , maxTimesTaken : Maybe Int
    , mustHaves : Set ModifierName
    , mustNotHaves : Set ModifierName
    }

qualityRequirements : XmlContentDecoder Requirements
qualityRequirements =
    XD.succeed <| Requirements Always Nothing Set.empty Set.empty

sourceIt : XmlContentDecoder Sourcing
sourceIt =
    XD.map3 Sourcing
        (XD.tag "source" XD.string)
        (XD.tag "page" XD.int)
        (XD.maybeTag "nameonpage" XD.string)