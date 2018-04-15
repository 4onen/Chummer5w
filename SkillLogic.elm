module SkillLogic exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)

import Table

import PointBuy exposing (PointBuy(..))
import Priorities exposing (Priorities)
import Magicality exposing (Magicality)
import Attributes exposing (Attribute(..), AttrObj)

import Skills exposing (..)
import SkillGroups exposing (Group)

type alias Model =
    { skills : Dict Skill Int
    , groups : Dict Group Int
    , searchQuery : String
    , skillTableState : Table.State
    , groupTableState : Table.State
    }

default : Model
default =
    { skills = 
        all
            |> List.map ((flip (,)) 0)
            |> Dict.fromList
    , groups = 
        SkillGroups.getCompleteGroupList
            |> List.map ((flip (,)) 0) 
            |> Dict.fromList
    , searchQuery = ""
    , skillTableState = Table.initialSort "Skill"
    , groupTableState = Table.initialSort "Group"
    }

type Msg
    = SetQuery String
    | PointTableState Table.State
    | PointChange (PointBuy Skill)
    | GroupPointTableState Table.State
    | GroupPointChange (PointBuy String)

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetQuery newQuery ->
            {model|searchQuery = newQuery}
        PointChange change ->
            {model|skills = changeSkillPoint change model.skills}
        GroupPointChange change ->
            {model|groups = changeGroupPoint change model.groups}
        PointTableState newState ->
            {model|skillTableState = newState}
        GroupPointTableState newState ->
            {model|groupTableState = newState}

changeGroupPoint : PointBuy Group -> (Dict Group Int -> Dict Group Int)
changeGroupPoint change =
    case change of
        Buy group ->
            Dict.update group (Maybe.map ((+) 1))
        Sell group ->
            Dict.update group (Maybe.map (\v -> if v>0 then v-1 else 0))
        Set group val ->
            Dict.update group (Maybe.map (always val))

changeSkillPoint : PointBuy Skill -> (Dict Skill Int -> Dict Skill Int)
changeSkillPoint change =
    case change of
        Buy skill ->
            Dict.update skill (Maybe.map ((+) 1))
        Sell skill ->
            Dict.update skill (Maybe.map (\v -> if v>0 then v-1 else 0))
        Set skill val ->
            Dict.update skill (Maybe.map (always val))

magicChanged : Magicality -> Int -> Model -> Model
magicChanged mag i ({skills,groups} as model) =
    let
        zeroSkillByAttribute attr = 
            getAttributeSkills attr
                |> List.map (\s -> Dict.update s (Maybe.map (always 0)))
                |> List.foldl (>>) identity
        zeroSkillByAttributes attrs =
            attrs
                |> List.map zeroSkillByAttribute
                |> List.foldl (>>) identity
        zeroMagicGroups =
            Dict.update "Conjuring" (Maybe.map (always 0))
                >> Dict.update "Enchanting" (Maybe.map (always 0))
                >> Dict.update "Sorcery" (Maybe.map (always 0))
        zeroResGroups = 
            Dict.update "Tasking" (Maybe.map (always -1))
        newSkills = 
            if i<1 then
                zeroSkillByAttributes [Attributes.MAG,Attributes.RES] skills
            else
                case mag of
                    Magicality.Mundane ->
                        zeroSkillByAttributes [Attributes.MAG,Attributes.RES] skills
                    Magicality.Technomancer ->
                        zeroSkillByAttribute Attributes.MAG skills
                    _ ->
                        zeroSkillByAttribute Attributes.RES skills
        newGroups =
            if i<1 then
                (zeroMagicGroups >> zeroResGroups) groups
            else
                case mag of
                    Magicality.Mundane ->
                        (zeroMagicGroups >> zeroResGroups) groups
                    Magicality.Technomancer ->
                        zeroMagicGroups groups
                    _ ->
                        zeroResGroups groups
    in
        {model|skills=newSkills,groups=newGroups}

view : Priorities -> AttrObj -> Magicality -> Model -> List (Html Msg)
view ps attrs magicality model = 
    model
        |> viewSelector ps attrs magicality
        |> realView

type alias ViewSkillEntry =
    { skill : Skill
    , skillGroup : Group
    , groupVal : Int
    , pointsBought : Int
    , attr : Attribute
    , attrVal : Int
    , rating : Int
    }

type alias ViewModel =
    { attrs : AttrObj
    , allowedSkills : List ViewSkillEntry
    , allowedGroups : List (String,Int)
    , skillPoints : Int
    , groupPoints : Int
    , searchQuery : String
    , skillTableState : Table.State
    , skillTableConfig : Table.Config ViewSkillEntry Msg
    , groupTableState : Table.State
    , groupTableConfig : Table.Config (Group,Int) Msg
    }


viewSelector : Priorities -> AttrObj -> Magicality -> Model -> ViewModel
viewSelector ps attrs magicality {skills,groups,searchQuery,skillTableState,groupTableState} =
    let
        (skillPoints,groupPoints) = getSkillAndGroupPointCount ps
        skillEntryToSkillView (skill,pointsBought) =
            let
                attr = getSkillAttribute skill
                attrVal = Attributes.get attr attrs
                group = Maybe.withDefault "" <| SkillGroups.getGroup skill
                groupVal = Maybe.withDefault 0 <| Dict.get group groups
                rating = 
                    if (pointsBought+groupVal > 0) then
                        attrVal+groupVal+pointsBought
                    else if (List.member skill canDefault) then
                        attrVal-1
                    else
                        0
            in
                ViewSkillEntry skill group groupVal pointsBought attr attrVal rating
        viewSkillEntries = 
            skills
                |> Dict.toList
                |> List.map skillEntryToSkillView
        groupData = 
            Dict.toList groups 
                |> List.filterMap
                    ( case magicality of
                        Magicality.Mundane ->
                            (\val ->
                                case Tuple.first val of 
                                    "Conjuring" -> Nothing
                                    "Enchanting" -> Nothing
                                    "Sorcery" -> Nothing
                                    "Tasking" -> Nothing
                                    _ -> Just val
                            )
                        Magicality.Technomancer ->
                            (\val ->
                                case Tuple.first val of
                                    "Conjuring" -> Nothing
                                    "Enchanting" -> Nothing
                                    "Sorcery" -> Nothing
                                    _ -> Just val
                            )
                        _ ->
                            (\val ->
                                case Tuple.first val of 
                                    "Tasking" -> Nothing
                                    _ -> Just val
                            )
                    )
    in
        { attrs = attrs
        , allowedSkills = viewSkillEntries
        , allowedGroups = groupData
        , skillPoints = skillPoints
        , groupPoints = groupPoints
        , searchQuery = searchQuery
        , skillTableState = skillTableState
        , skillTableConfig =
            Table.config
                { toId = .skill>>Basics.toString
                , toMsg = PointTableState
                , columns =
                    [ Table.stringColumn "Skill" (.skill)
                    , Table.stringColumn "Attribute" (.attr>>Basics.toString)
                    , Table.stringColumn "Group" (\s -> s.skillGroup ++ " " ++ (if s.groupVal>0 then Basics.toString s.groupVal else ""))
                    , pointSpendColumn
                    , Table.stringColumn "Rating" (.rating>>(\r -> if (r > -1) then toString r else "Unaware"))
                    ]
                }
        , groupTableState = groupTableState
        , groupTableConfig =
            Table.config
                { toId = (Tuple.first)
                , toMsg = GroupPointTableState
                , columns =
                    [ Table.stringColumn "Group" (Tuple.first)
                    , groupPointColumn
                    ]
                }
        }

realView : ViewModel -> List (Html Msg)
realView model = 
    [ Html.details []
        [ Html.summary [] [Html.text "Groups"]
        , Html.div [] [Html.text ("Skill Group Points "++(toString (model.groupPoints - (List.sum (List.map Tuple.second model.allowedGroups)))))]
        , Table.view model.groupTableConfig model.groupTableState model.allowedGroups
        ]
    , Html.details []
        [ Html.summary [] [Html.text "Skills"]
        , Html.div [] [Html.text ("Skill Points "++(toString (model.skillPoints - (List.sum (List.map .pointsBought model.allowedSkills)))))]
        , Table.view model.skillTableConfig model.skillTableState model.allowedSkills
        ]
    ]
    

pointSpendColumn : Table.Column ViewSkillEntry Msg
pointSpendColumn = 
    Table.veryCustomColumn
        { name = "Points"
        , viewData =
            (\data ->
                { attributes = []
                , children = 
                    data.pointsBought
                        |> PointBuy.buyInterfaceWithDisable (data.attrVal<1) data.skill
                        |> List.map (Html.map PointChange)
                }
            )
        , sorter = Table.increasingOrDecreasingBy .pointsBought
        }

groupPointColumn : Table.Column (Group, Int) Msg
groupPointColumn =
    Table.veryCustomColumn
        { name = "Points"
        , viewData = 
            (\(groupStr, val) ->
                { attributes = []
                , children = 
                    val
                        |> PointBuy.buyInterface groupStr 
                        |> List.map (Html.map GroupPointChange)
                }
            )
        , sorter = Table.increasingOrDecreasingBy Tuple.second
        }
