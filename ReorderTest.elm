module ReorderTest exposing (main)

import Html exposing (Html)
import Html.Events
import Time
import Char

import ReorderableList

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = 
    { rl : ReorderableList.Model
    , list : List String
    , reorder : Bool
    , extra : Bool
    }

init : (Model, Cmd Msg)
init = 
    ( Model 
        ReorderableList.init
        ["Alpha","Beta","Tomato","Delta","Grammar","Hardy"]
        True
        False
    ) ! []

type Msg
    = RLMsg ReorderableList.Msg
    | ToggleReorderEnabled
    | ToggleExtraElement Time.Time
    | ToggleToggleExtraElement

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RLMsg m ->
            let
                (newRl,newList) = ReorderableList.update m model.rl model.list
            in
                {model | rl=newRl, list = newList} ! []
        ToggleReorderEnabled ->
            {model | reorder = not model.reorder} ! []
        ToggleExtraElement _ ->
            case model.list of
                "Zeta"::rest ->
                    {model|list=rest,rl=ReorderableList.killDrag model.rl} ! []
                rest ->
                    {model|list="Zeta"::rest,rl=ReorderableList.killDrag model.rl} ! []
        ToggleToggleExtraElement ->
            {model | extra = not model.extra} ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ( [ (Sub.map RLMsg <| ReorderableList.subscriptions model.rl)
          ] ++ (
            if model.extra then
                List.singleton <| Time.every Time.second ToggleExtraElement
            else
                []
          )
        )


view : Model -> Html Msg
view {rl,list,reorder,extra} =
    Html.div [] 
        [ Html.button [Html.Events.onClick ToggleReorderEnabled] [Html.text (if reorder then "Disable Drag" else "Drag")]
        , Html.button [Html.Events.onClick ToggleToggleExtraElement] [Html.text (if extra then "Disable Extra" else "Extra! Extra!")]
        , ReorderableList.viewWithOptions RLMsg (ReorderableList.Options reorder (Just viewListTag) (Just (\i v -> Html.text v))) rl list
        ]

viewListTag : Int -> Html Msg
viewListTag idx =
    'A'
        |> Char.toCode
        |> (+) idx
        |> Char.fromCode
        |> List.singleton
        |> String.fromList
        |> Html.text
