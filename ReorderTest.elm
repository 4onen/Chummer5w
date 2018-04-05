module ReorderTest exposing (main)

import Html exposing (Html)
import Html.Events

import ReorderableList

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = { rl : ReorderableList.Model String, reorder : Bool }

init = 
    ( Model 
        (ReorderableList.fromList ["Alpha","Beta","Tomato","Delta","Grammar","Hardy"])
        True
    ) ! []

type Msg
    = RLMsg ReorderableList.Msg
    | ToggleReorderEnabled

update msg model =
    case msg of
        RLMsg m ->
            {model | rl=ReorderableList.update m model.rl} ! []
        ToggleReorderEnabled ->
            {model | reorder = not model.reorder} ! []
 
subscriptions model =
    Sub.map RLMsg <| ReorderableList.subscriptions model.rl

view {rl,reorder} =
    Html.div [] 
        [ Html.button [Html.Events.onClick ToggleReorderEnabled] [Html.text (if reorder then "Disable Drag" else "Drag")]
        , Html.map RLMsg <| ReorderableList.viewWithEnable reorder rl
        ]