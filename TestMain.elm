module TestMain exposing (main)

import Html exposing (Html)
import Html.Events
import Html.Attributes
import Debug

import Character exposing (Character)
import Metatype
import Attributes exposing (AttributeObject)
import Priorities exposing (Priorities)

type alias Model = Character

type Msg 
    = SpendPoint Attributes.Label
    | UnspendPoint Attributes.Label
    | PrioritySwap Int Int
    | PriorityRaise Int
    | PriorityLower Int

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : (Model, Cmd Msg)
init = 
    (Character.default, Cmd.none)

noCmdWrap : a -> (a,Cmd msg)
noCmdWrap mdl = (mdl,Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg oldModel =
    let
        model = {oldModel | mostRecentError = ""}
    in
        case msg of
            SpendPoint lbl ->
                Character.spendPoint lbl model |> noCmdWrap
            UnspendPoint lbl ->
                Character.unspendPoint lbl model |> noCmdWrap
            PrioritySwap i1 i2 -> 
                {model | priorities = Priorities.swap i1 i2 model.priorities} 
                    |> noCmdWrap
            PriorityRaise i ->
                {model | priorities = Priorities.raise i model.priorities}
                    |> noCmdWrap
            PriorityLower i ->
                {model | priorities = Priorities.lower i model.priorities}
                    |> noCmdWrap


view : Model -> Html Msg
view model =
    Html.div [] 
        [ viewPriorities model.priorities
        , viewAttributes model
        , Html.text model.mostRecentError
        ]

viewPriorities : Priorities -> Html Msg
viewPriorities ps =
    let
        imap = 
            (\idx plist -> 
                let
                    stringAndIndex = List.indexedMap (,) plist
                    currentPriorityTag = 
                        stringAndIndex 
                            |> List.foldl (\(i,v) acc -> if i==idx then (i,v) else acc) (-1,"NOT_FOUND")
                            |> Tuple.mapFirst (\i->i|>Priorities.indexToPriorityChar|>Priorities.priorityCharToString)
                            |> (\(c,s)->Html.text (c++" -- "++s))
                            --|> (\t -> Html.p [] [t])
                    buttons = 
                        stringAndIndex
                            |> List.map (\(i,_) -> 
                                Html.button 
                                    [ Html.Attributes.disabled (i==idx) 
                                    , Html.Events.onClick (PrioritySwap idx i)
                                    ] 
                                    [ i |> Priorities.indexToPriorityChar
                                        |> Priorities.priorityCharToString
                                        |> Html.text 
                                    ])
                    upDownButtons =
                        upDownButton (PriorityRaise idx,"^",idx==0) (PriorityLower idx,"v",idx==4)
                in
                    Html.div [] (buttons++upDownButtons++[currentPriorityTag])
            )
    in
        ps  |> Priorities.getList
            |> List.map Basics.toString
            |> List.repeat 5
            |> List.indexedMap imap
            |> (::) (Html.h1 [] [Html.text "Priorities"])
            |> Html.div [] 

viewAttributes : Model -> Html Msg
viewAttributes model =
    let
        magresPriority = Priorities.getChar Priorities.Talent model.priorities
        co = Character.getAttributeObject model
        maxo = Metatype.getMaxStats model.race magresPriority
        lst = Debug.log "viewAttrLst:" <| List.map (\f -> (f co,f maxo)) Attributes.getterList
        htmlLst =
            lst
                |> List.map (\(c,max) -> (c,c>max))
                |> List.map (Tuple.mapFirst (Basics.toString>>Html.text))
                |> List.map (\(t,overMax) -> 
                                Html.td 
                                    (if overMax then [Html.Attributes.style [("background-color","red")]] else [])
                                    [t]
                            )
                |> List.map 
                    (\td ->
                        Html.td []
                            (upDownButton 
                                (SpendPoint (Attributes.Base Attributes.AGILITY)
                                ,"+"
                                ,False
                                ) 
                                (UnspendPoint (Attributes.Base Attributes.AGILITY)
                                ,"-"
                                ,False
                                )
                            )::td::[]
                    )
                |> List.map (\t -> Html.tr [] t)
    in
        Html.table [] htmlLst

upDownButton : (Msg,String,Bool) -> (Msg,String,Bool) -> List (Html Msg)
upDownButton (upMsg,upTag,upDisabled) (downMsg,downTag,downDisabled) =
    let
        makeButton msg tag bool =
            Html.button [Html.Events.onClick msg, Html.Attributes.disabled bool] [Html.text tag]
    in
        [makeButton upMsg upTag upDisabled
        ,makeButton downMsg downTag downDisabled
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
