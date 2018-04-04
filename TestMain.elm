module TestMain exposing (main)

import Html exposing (Html)
import Html.Events
import Html.Attributes

import Character exposing (Character)
import Attributes exposing (AttributeObject)
import Priorities exposing (Priorities)

type alias Model = Character

type Msg 
    = SpendPoint Attributes.Label
    | UnspendPoint Attributes.Label
    | PrioritySwap Int String

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
update msg model =
    case msg of
        SpendPoint lbl ->
            Character.spendPoint lbl model |> noCmdWrap
        UnspendPoint lbl ->
            Character.unspendPoint lbl model |> noCmdWrap
        PrioritySwap i1 s2 -> 
            case String.toInt s2 of
                Result.Ok i2 ->
                    {model | priorities = Priorities.swap i1 i2 model.priorities} 
                        |> noCmdWrap
                Result.Err message ->
                    {model | mostRecentError = message} |> noCmdWrap


view : Model -> Html Msg
view model =
    Html.div [] 
        [ viewPriorities model.priorities
        , viewAttributes model]

viewPriorities : Priorities -> Html Msg
viewPriorities ps =
    let
        innerimap = 
            (\selected idx item ->
                Html.option 
                    [ Html.Attributes.selected (selected==idx)
                    , Html.Attributes.value (toString idx)
                    ]
                    [ Html.text item ]
            )
        imap = 
            (\idx plist -> 
                plist
                    |> List.indexedMap (innerimap idx)
                    |> Html.select [Html.Events.onInput (PrioritySwap idx)]
                    
            )
    in
        ps  |> Priorities.getList
            |> List.map Priorities.priorityToString
            |> List.repeat 5
            |> List.indexedMap imap
            |> (::) (Html.p [] [Html.text "Priorities"])
            |> Html.div [] 

viewAttributes : Model -> Html Msg
viewAttributes model =
    let
        ao = Character.getAttributeObject model
        lst = List.map (\f -> f ao) Attributes.getterList
        strList = List.map Basics.toString lst
        htmlList = List.map Html.text strList
    in
        Html.div [] htmlList


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
