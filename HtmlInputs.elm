module HtmlInputs exposing (..)

import Html exposing (Html)
import Html.Events
import Html.Attributes

viewCheckbox : String -> msg -> Bool -> Html msg
viewCheckbox label msg checked =
    Html.label []
        [ Html.input 
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked checked
            , Html.Events.onClick msg] []
        , Html.text label
        ]

viewRadioButtons : List String -> List msg -> Int -> Html msg
viewRadioButtons labels msgs selected =
    let
        radio lbl msg idx =
            Html.label 
                [ Html.Attributes.style [("display","block")]]
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.checked <| idx==selected
                    , Html.Events.onClick msg
                    ] []
                , Html.text lbl
                ]
        len = max (List.length labels) (List.length msgs)
    in
        List.map3 radio 
            labels 
            msgs 
            (List.range 0 len)
        |> Html.fieldset []
