module SkillLogic exposing (..)

import Html exposing (Html)

import Table

import PointBuy exposing (PointBuy(..))
import Priorities exposing (Priorities)
import Magicality exposing (Magicality)
import Attributes exposing (Attribute(..), AttrObj)

import Skills exposing (..)
import SkillGroups exposing (..)

type alias SkillEntry =
    { skill : Skill
    , pointsBought : Int
    }

type alias Model =
    { skills : List SkillEntry
    , searchQuery : String
    , tableState : Table.State
    }

default : Model
default =
    Model
        (List.map ((flip SkillEntry) 0) all)
        ("")
        (Table.initialSort "Rating")


type Msg
    = SetQuery String
    | SetTableState Table.State
    | PointChange (PointBuy Skill)

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetQuery newQuery ->
            {model|searchQuery = newQuery}
        SetTableState newState ->
            {model|tableState = newState}
        PointChange change ->
            {model|skills = changeSkillPoint change model.skills}

changeSkillPoint : PointBuy Skill -> (List SkillEntry -> List SkillEntry)
changeSkillPoint change =
    case change of
        Buy skill ->
            List.map 
                (\s -> 
                    if s.skill==skill then 
                        {s|pointsBought = s.pointsBought+1}
                    else
                        s
                )
        Sell skill ->
            List.map
                (\s -> 
                    if s.skill==skill then 
                        {s|pointsBought = s.pointsBought-1}
                    else
                        s
                )
        Set skill val ->
            List.map
                (\s -> 
                    if s.skill==skill then 
                        {s|pointsBought = s.pointsBought-1}
                    else
                        s
                )

view : Priorities -> AttrObj -> Magicality -> Model -> Html Msg
view ps attrs magicality model = 
    model
        |> viewSelector ps attrs magicality
        |> realView

type alias ViewSkillEntry =
    { skill : Skill
    , skillGroup : String
    , pointsBought : Int
    , attr : Attribute
    , attrVal : Int 
    }

type alias ViewModel =
    { attrs : AttrObj
    , allowedSkills : List ViewSkillEntry
    , skillPoints : Int
    , groupPoints : Int
    , searchQuery : String
    , tableState : Table.State
    , tableConfig : Table.Config ViewSkillEntry Msg
    }


viewSelector : Priorities -> AttrObj -> Magicality -> Model -> ViewModel
viewSelector ps attrs magicality {skills,searchQuery,tableState} =
    let
        (skillPoints,groupPoints) = getSkillAndGroupPointCount ps
        skillEntryToSkillView {skill,pointsBought} =
            let
                attr = getSkillAttribute skill
                attrVal = Attributes.get attr attrs
                group = Maybe.withDefault "<None>" <| SkillGroups.getGroup skill
            in
                ViewSkillEntry skill group pointsBought attr attrVal
        viewSkillEntries = List.map skillEntryToSkillView skills
    in
        ViewModel
            ( attrs )
            ( viewSkillEntries )
            ( skillPoints )
            ( groupPoints )
            ( searchQuery )
            ( tableState )
            ( Table.config 
                { toId = .skill>>Basics.toString
                , toMsg = SetTableState
                , columns =
                    [ Table.stringColumn "Skill" (.skill>>Basics.toString)
                    , Table.stringColumn "Group" (.skillGroup)
                    , Table.stringColumn "Attribute" (.attr>>Basics.toString)
                    , Table.intColumn "Rating" (\vs -> vs.attrVal + vs.pointsBought)
                    , pointSpendColumn
                    ]
                }
            )


realView : ViewModel -> Html Msg
realView model = 
    Html.details []
        [ Html.summary [] [Html.text "Skills"]
        , Table.view model.tableConfig model.tableState model.allowedSkills
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
                        |> PointBuy.buyInterface data.skill
                        |> List.map (Html.map PointChange)
                }
            )
        , sorter = Table.increasingOrDecreasingBy .pointsBought
        }