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
        , ReorderableList.viewWithOptions RLMsg (ReorderableList.Options reorder (Just viewListTag) (Just Html.text) (Just viewRaiseLowerButton)) rl
        ]

viewListTag : Int -> Html Msg
viewListTag idx =
    ( case idx of
        0 -> "A"
        1 -> "B"
        2 -> "C"
        3 -> "D"
        4 -> "E"
        _ -> "ERR"
    ) |> Html.text

viewRaiseLowerButton : Int -> a -> Bool -> Html Msg
viewRaiseLowerButton _ _ up =
    if up then
        Html.text "^"
    else
        Html.text "v"