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

viewRadioButtons : List String -> List msg -> msg -> Html msg
viewRadioButtons labels msgs msgSelected =
    let
        radio lbl msg =
            Html.label 
                [ Html.Attributes.style [("display","block")]]
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.checked <| msg==msgSelected
                    , Html.Events.onClick msg
                    ] []
                , Html.text lbl
                ]
        len = max (List.length labels) (List.length msgs)
    in
        List.map2 radio 
            labels 
            msgs
        |> Html.fieldset []
