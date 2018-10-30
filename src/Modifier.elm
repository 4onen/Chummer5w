module Modifier exposing (CanPurchase(..), EffectCategory(..), Effects, Id, Modifier, ModifierName, Requirements, Sourcing, qualityEffects, qualityModifier, qualityRequirements, sourceIt)

import Dict exposing (Dict)
import Set exposing (Set)
import Xml.Decode as XD exposing (Decoder)


type CanPurchase
    = CharGenOnly
    | CampaignOnly
    | Always


type alias Sourcing =
    { sourceBook : String
    , sourcePage : Int
    , nameOnPage : Maybe String
    }


type alias ModifierName =
    String


type alias Id =
    String


type alias Modifier =
    { id : Id
    , name : ModifierName
    , effects : Effects
    , requirements : Requirements
    , source : Sourcing
    }


qualityModifier : Decoder Modifier
qualityModifier =
    XD.map5 Modifier
        (XD.path [ "id" ] (XD.single XD.string) |> XD.map String.toLower)
        (XD.path [ "name" ] (XD.single XD.string))
        qualityEffects
        qualityRequirements
        sourceIt


type EffectCategory
    = Positive
    | Negative
    | Undefined


type alias Effects =
    { karmaChange : Int
    , chargenEffets : Dict String Int
    , alwaysEffects : Dict String Int
    , effectCategory : EffectCategory
    }


qualityEffects : Decoder Effects
qualityEffects =
    XD.map4 Effects
        (XD.map negate <| XD.path [ "karma" ] <| XD.single XD.int)
        (XD.succeed Dict.empty)
        (XD.succeed Dict.empty)
        (XD.path [ "category" ] (XD.single XD.string)
            |> XD.map
                (\str ->
                    case str of
                        "Positive" ->
                            Positive

                        "Negative" ->
                            Negative

                        _ ->
                            Undefined
                )
        )


type alias Requirements =
    { canPurchase : CanPurchase
    , maxTimesTaken : Maybe Int
    , mustHaves : Set ModifierName
    , mustNotHaves : Set ModifierName
    }


qualityRequirements : Decoder Requirements
qualityRequirements =
    XD.succeed <| Requirements Always Nothing Set.empty Set.empty


sourceIt : Decoder Sourcing
sourceIt =
    XD.map3 Sourcing
        (XD.path [ "source" ] <| XD.single XD.string)
        (XD.path [ "page" ] <| XD.single XD.int)
        (XD.possiblePath [ "nameonpage" ] (XD.single XD.string) (XD.succeed identity))
