module Character exposing (..)

import Html exposing (Html)
import Html.Attributes

import HtmlInputs

import ReorderableList
import PointBuy exposing (PointBuy(..))
import Priorities exposing (Priorities)
import Magicality exposing (Magicality(..))
import Metatype exposing (Metatype(..))
import Attributes exposing (Attribute, AttrObj, attrObj)
import AttributesView
import SkillLogic

type alias Character =
    { name : String
    , priorities : Priorities
    , prioritiesLocked : Bool
    , magicality : Magicality
    , race : Metatype
    , attributes : AttrObj
    , skills : SkillLogic.Model
    }

type Msg
    = PrioritiesMsg Priorities.Msg
    | TogglePriorityLock
    | ChangeMagicality Magicality
    | ChangeRace Metatype
    | AttrPoint (PointBuy Attribute)
    | SkillMsg SkillLogic.Msg

default : Character
default =
    Character
        ""
        Priorities.default
        False
        Magicality.Magician
        (Human Nothing)
        (attrObj 0 0 0 0 0 0 0 0 0 0 0)
        (SkillLogic.default)

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
            {model|magicality = new}
        ChangeRace new ->
            {model|race = new}
        AttrPoint (Buy attr) ->
            {model|attributes = Attributes.increase attr model.attributes}
        AttrPoint (Sell attr) ->
            {model|attributes = Attributes.decrease attr model.attributes}
        AttrPoint (Set attr val) ->
            {model|attributes = Attributes.set attr val model.attributes}
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
        newMagicAttr = 
            if newMagicPriorityIdx>3 
                || (newMagicPriorityIdx==3 && (model.magicality/=Magicality.Adept && model.magicality/=Magicality.AspectedMagician))
                || (newMagicPriorityIdx==0 && (model.magicality==Magicality.Adept || model.magicality==Magicality.AspectedMagician))
            then
                Attributes.set Attributes.RES 0 <| Attributes.set Attributes.MAG 0 <| model.attributes
            else
                model.attributes
    in
        {model
            | priorities = newPriorities
            , race = newRace
            , attributes = newMagicAttr
            }

type alias ViewCharacter =
    { name : String
    , priorities : Priorities
    , prioritiesLocked : Bool
    , magicality : Magicality
    , magicPriority : Int
    , magicRating : Int
    , race : Metatype
    , racePriority : Int
    , baseAttributes : AttrObj
    , boughtAttributes : AttrObj
    , maxAttributes : AttrObj
    , attributePointCount : Int
    , skills : SkillLogic.Model
    }

viewSelector : Character -> ViewCharacter
viewSelector chr =
    let
        racePriority = Priorities.getPriorityIndex Priorities.Metatype chr.priorities
        magicPriority = Priorities.getPriorityIndex Priorities.MagicOrResonance chr.priorities
        magicRating = Tuple.second <| Magicality.getBaseMagicality chr.magicality magicPriority
        raceBaseAttributes = Metatype.getBaseStats chr.race (chr.magicality,magicRating)
        raceMaxAttributes = Metatype.getMaxStats chr.race (chr.magicality,magicRating)
    in
        ViewCharacter
            ( chr.name )
            ( chr.priorities )
            ( chr.prioritiesLocked )
            ( chr.magicality )
            ( magicPriority )
            ( magicRating )
            ( chr.race )
            ( racePriority )
            ( raceBaseAttributes )
            ( chr.attributes )
            ( raceMaxAttributes )
            ( Attributes.getPriorityPointCount chr.priorities )
            ( chr.skills )


view : ViewCharacter -> Html Msg
view model =
    Html.div 
        [ Html.Attributes.style
            [("display","flex")
            ,("flex-wrap","wrap")
            ,("justify-content","space-around")]
        ] 
        [ viewPriorities model.prioritiesLocked model.magicality model.priorities
        , viewMagicalityList model.magicPriority model.magicality
        , viewMetatypeSelection model.priorities model.race
        , Html.map AttrPoint <| AttributesView.view model.baseAttributes model.boughtAttributes model.maxAttributes model.attributePointCount
        , Html.map SkillMsg <| SkillLogic.view model.priorities (Attributes.add model.baseAttributes model.boughtAttributes) model.magicality model.skills
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
        magList = if magicPriority==4 then [Magicality.Adept] else Magicality.listOfTypes
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
