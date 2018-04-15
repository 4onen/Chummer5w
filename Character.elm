module Character exposing (..)

import Html exposing (Html)
import Html.Attributes

import HtmlInputs

import ReorderableList
import PointBuy
import Priorities exposing (Priorities)
import Magicality exposing (Magicality(..))
import Metatype exposing (Metatype(..))
import Attributes exposing (Attribute, AttrObj, attrObj)
import AttributesView
import SkillLogic

type alias Character =
    { name : String
    , baseKarma : Int
    , priorities : Priorities
    , prioritiesLocked : Bool
    , magicality : Magicality
    , race : Metatype
    , attributes : AttrObj
    , karmaAttributes : AttrObj
    , skills : SkillLogic.Model
    }

type Msg
    = PrioritiesMsg Priorities.Msg
    | TogglePriorityLock
    | ChangeMagicality Magicality
    | ChangeRace Metatype
    | AttrPoint (Attribute,Int)
    | KAttrPoint (Attribute,Int)
    | SkillMsg SkillLogic.Msg

default : Character
default =
    { name = ""
    , baseKarma = 25
    , priorities = Priorities.default
    , prioritiesLocked = False
    , magicality = Magicality.Magician
    , race = (Human Nothing)
    , attributes = (attrObj 0 0 0 0 0 0 0 0 0 0 0)
    , karmaAttributes = (attrObj 0 0 0 0 0 0 0 0 0 0 0)
    , skills = (SkillLogic.default)
    }

update : Msg -> Character -> Character
update msg model =
    case msg of
        PrioritiesMsg (ReorderableList.DragEnd m) ->
            updateModelBasedOnNewPriorities (ReorderableList.DragEnd m) model
        PrioritiesMsg (ReorderableList.RaiseItem m) ->
            updateModelBasedOnNewPriorities (ReorderableList.RaiseItem m) model
        PrioritiesMsg (ReorderableList.LowerItem m) ->
            updateModelBasedOnNewPriorities (ReorderableList.LowerItem m) model
        PrioritiesMsg m ->
            {model|priorities = Priorities.update m model.priorities}
        TogglePriorityLock ->
            {model|prioritiesLocked = not model.prioritiesLocked}
        ChangeMagicality new ->
            updateModelBasedOnNewMagicality new model
        ChangeRace new ->
            {model|race = new}
        AttrPoint (attr,val) ->
            {model|attributes = Attributes.set attr val model.attributes}
        KAttrPoint (attr,val) ->
            {model|karmaAttributes = Attributes.set attr val model.karmaAttributes}
        SkillMsg msg ->
            {model|skills = SkillLogic.update msg model.skills}

updateModelBasedOnNewPriorities : Priorities.Msg -> Character -> Character
updateModelBasedOnNewPriorities m model =
    let 
        newPriorities = Priorities.update m model.priorities
        newRace = 
            if (List.member model.race <| Metatype.getAllowed <| Priorities.getPriorityIndex Priorities.Metatype newPriorities) then
                model.race
            else
                Metatype.Human Nothing
        newMagicPriorityIdx = Priorities.getPriorityIndex Priorities.MagicOrResonance newPriorities
        newMagicality = 
            if not <| List.member model.magicality (Magicality.getTypesByPriorityIdx newMagicPriorityIdx) then
                case List.head (Magicality.getTypesByPriorityIdx newMagicPriorityIdx) of
                    Just n -> n
                    Nothing -> Magicality.Mundane
            else
                model.magicality
        (newMagicAttr,newMagicKAttr) = 
            if newMagicPriorityIdx>3 
                || (newMagicality == Magicality.Mundane)
                || (newMagicPriorityIdx==3 && (newMagicality/=Magicality.Adept && newMagicality/=Magicality.AspectedMagician))
                || (newMagicPriorityIdx==0 && (newMagicality==Magicality.Adept || newMagicality==Magicality.AspectedMagician))
            then
                ( Attributes.set Attributes.RES 0 << Attributes.set Attributes.MAG 0 <| model.attributes
                , Attributes.set Attributes.RES 0 << Attributes.set Attributes.MAG 0 <| model.karmaAttributes
                )
            else
                (model.attributes,model.karmaAttributes)
        newMagicSkills =
            if newMagicality/=model.magicality then
                SkillLogic.magicChanged newMagicality (max (Attributes.get Attributes.MAG newMagicAttr) (Attributes.get Attributes.RES newMagicAttr))
            else
                identity
    in
        {model
            | priorities = newPriorities
            , magicality = newMagicality
            , race = newRace
            , attributes = newMagicAttr
            , karmaAttributes = newMagicKAttr
            , skills = newMagicSkills model.skills
            }

updateModelBasedOnNewMagicality : Magicality -> Character -> Character
updateModelBasedOnNewMagicality newMagicality model =
    let
        newMagicAttr = 
            case newMagicality of
                Magicality.Mundane ->
                    (Attributes.set Attributes.MAG 0 >> Attributes.set Attributes.RES 0) model.attributes
                Magicality.Technomancer ->
                    Attributes.set Attributes.MAG 0 model.attributes
                _ ->
                    Attributes.set Attributes.RES 0 model.attributes
        newMagicKAttr =
            case newMagicality of
                Magicality.Mundane ->
                    (Attributes.set Attributes.MAG 0 >> Attributes.set Attributes.RES 0) model.karmaAttributes
                Magicality.Technomancer ->
                    Attributes.set Attributes.MAG 0 model.karmaAttributes
                _ ->
                    Attributes.set Attributes.RES 0 model.karmaAttributes
        newSkills =
            case newMagicality of 
                Magicality.Technomancer ->
                    SkillLogic.magicChanged newMagicality (Attributes.get Attributes.RES newMagicAttr) model.skills
                _ ->
                    SkillLogic.magicChanged newMagicality (Attributes.get Attributes.MAG newMagicAttr) model.skills
    in
        {model
                | magicality = newMagicality
                , attributes = newMagicAttr
                , karmaAttributes = newMagicKAttr
                , skills = newSkills
                }

type alias ViewCharacter =
    { name : String
    , karma : Int
    , priorities : Priorities
    , prioritiesLocked : Bool
    , magicality : Magicality
    , magicPriority : Int
    , magicRating : Int
    , race : Metatype
    , racePriority : Int
    , baseAttributes : AttrObj
    , boughtAttributes : AttrObj
    , karmaAttributes : AttrObj
    , maxAttributes : AttrObj
    , attributePointCount : Int
    , spAttributePointCount : Int
    , skills : SkillLogic.Model
    }

viewSelector : Character -> ViewCharacter
viewSelector chr =
    let
        racePriority = Priorities.getPriorityIndex Priorities.Metatype chr.priorities
        magicPriority = Priorities.getPriorityIndex Priorities.MagicOrResonance chr.priorities
        magicRating = Tuple.second <| Magicality.getBaseMagicality chr.magicality magicPriority
        raceBaseAttributes = Metatype.getBaseStats chr.race (chr.magicality,magicPriority)
        raceMaxAttributes = Metatype.getMaxStats chr.race (chr.magicality,magicPriority)
        karma =
            (chr.baseKarma)-
            (Attributes.calculateKarma raceBaseAttributes chr.attributes chr.karmaAttributes)
    in
        ViewCharacter
            ( chr.name )
            ( karma )
            ( chr.priorities )
            ( chr.prioritiesLocked )
            ( chr.magicality )
            ( magicPriority )
            ( magicRating )
            ( chr.race )
            ( racePriority )
            ( raceBaseAttributes )
            ( chr.attributes )
            ( chr.karmaAttributes )
            ( raceMaxAttributes )
            ( Attributes.getPriorityPointCount chr.priorities )
            ( Metatype.getSpecialPoints racePriority chr.race )
            ( chr.skills )


view : ViewCharacter -> Html Msg
view model =
    Html.div [] 
        [ Html.div [] 
            [Html.text <| "Karma: "++(Basics.toString model.karma)]
        , Html.div 
            [ Html.Attributes.style
                [("display","flex")
                ,("flex-wrap","wrap")
                ,("justify-content","space-between")]
            ] 
            (
                [ viewPriorities model.prioritiesLocked model.magicality model.priorities
                , viewMagicalityList model.magicPriority model.magicality
                , viewMetatypeSelection model.priorities model.race
                , AttributesView.view model.baseAttributes model.boughtAttributes model.karmaAttributes model.maxAttributes model.attributePointCount model.spAttributePointCount AttrPoint KAttrPoint
                ] ++ ((List.map <| Html.map SkillMsg) <| SkillLogic.view model.priorities (Attributes.add model.baseAttributes model.boughtAttributes) model.magicality model.skills)
            )
        ]


viewPriorities : Bool -> Magicality -> Priorities -> Html Msg
viewPriorities locked magicality priorities =
    Html.details []
        [ Html.summary [] [Html.text "Priorities"]
        , Priorities.view (not locked) magicality PrioritiesMsg priorities
        , HtmlInputs.viewCheckbox "Priority rearrangement" TogglePriorityLock (not locked)
        ]


viewMagicalityList : Int -> Magicality -> Html Msg
viewMagicalityList magicPriority magicality =
    let
        magList = Magicality.getTypesByPriorityIdx magicPriority
        labelList = List.map ((flip Magicality.viewMagicClass) magicPriority) magList 
    in
        magicality
            |> HtmlInputs.viewRadioButtons labelList magList 
            |> Html.map ChangeMagicality
            |> List.singleton
            |> (::) (Html.summary [] [Html.text "Magic"])
            |> Html.details [] 

viewMetatypeSelection : Priorities -> Metatype -> Html Msg
viewMetatypeSelection ps currentRace =
    let
        metaPriorityIdx = Priorities.getPriorityIndex Priorities.Metatype ps
        allowedRaces = Metatype.getAllowed metaPriorityIdx
        metavariants = Metatype.getVariants currentRace
        metaToString m = 
            (Metatype.toString m)
                ++"("++
                    ( m |> Metatype.getSpecialPoints metaPriorityIdx 
                        |> Basics.toString
                    )
                ++")"
    in
        Html.map ChangeRace <| Html.details [] 
            [ Html.summary [] [Html.text "Metatype"]
            , HtmlInputs.viewRadioButtons (List.map metaToString allowedRaces) allowedRaces (Metatype.getBaseType currentRace)
            , HtmlInputs.viewRadioButtons (List.map metaToString metavariants) metavariants currentRace
            ]

subscriptions : Character -> Sub Msg
subscriptions model =
    Sub.map PrioritiesMsg <| Priorities.subscriptions model.priorities
