module ReorderableList exposing (..)

import Mouse
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import Json.Decode

type alias Model a =
    { data : List a
    , drag : Maybe Drag
    }

type alias Drag =
    { idx : Int
    , startY : Int
    , currentY : Int
    }

type alias Index = Int

init : Model a
init = fromList []

fromList : List a -> Model a
fromList input =
    {data = input, drag = Nothing}

toList : Model a -> List a
toList model =
    let
        safeModel = 
            case model.drag of
                Just d ->
                    updateDragEnd d.currentY model
                Nothing ->
                    model
    in
        safeModel.data

type Msg 
    = DragStart Index Int
    | DragAt Int
    | DragEnd Int

updateList : (List a -> List a) -> Model a -> Model a
updateList func model =
    {model | drag = Nothing, data = func model.data}

update : Msg -> Model a -> Model a
update msg model =
    case msg of
        DragStart idx y ->
            {model | drag = Just (Drag idx y y)}
        DragAt y ->
            {model | drag = Maybe.map (updateDragAt y) model.drag}
        DragEnd y ->
            (updateDragEnd y model)


updateDragAt : Int -> Drag -> Drag
updateDragAt y drag =
    {drag|currentY = y}


updateDragEnd : Int -> Model a -> Model a
updateDragEnd y model =
    case model.drag of
        Just d ->
            { model | data = moveItem d.idx ((d.currentY - d.startY + (if d.currentY < d.startY then -conservativeHalfHeight else conservativeHalfHeight))//elementHeight) model.data
            , drag = Nothing }
        Nothing ->
            model


moveItem : Index -> Index -> List a -> List a
moveItem from by oldList =
    let
        listWithoutMoved = (List.take from oldList) ++ (List.drop (from+1) oldList)
        moved = oldList |> List.drop from |> List.take 1
    in
        (List.take (from + by) listWithoutMoved)
            ++ moved
            ++ (List.drop (from + by) listWithoutMoved)


subscriptions : Model a -> Sub Msg
subscriptions {drag} =
    case drag of
        Just _ -> Sub.batch [Mouse.moves (.y>>DragAt), Mouse.ups (.y>>DragEnd)]
        Nothing -> Sub.none


viewWithEnable : Bool -> Model a -> Html Msg
viewWithEnable enable model =
    Html.table 
        [ Html.Attributes.style 
            [ ("margin","0")
            , ("padding","0")
            , ("list-style-type","none") 
            , ("vertical-align","middle")
            ]
        ]
        (List.indexedMap (itemView enable model) model.data)


itemView : Bool -> Model a -> Index -> a -> Html Msg
itemView enable {data,drag} myIdx item =
    let
        buttonStyle = 
            if enable then 
                [("display","inline-block")] 
            else 
                [("display","none")]
        moveStyle = 
            case drag of
                Just d ->
                    if d.idx == myIdx then
                        [ ("transform","translateY("++toString (d.currentY-d.startY)++"px) translateZ(10px)")
                        , ("box-shadow","0 3px 6px rgba(0,0,0,0.24)")
                        , ("will-change","transform")
                        ]
                    else
                        []
                Nothing ->
                    []
        makingWayStyle =
            case drag of
                Just d ->
                    if (myIdx<d.idx) && (d.currentY-d.startY)<(myIdx-d.idx)*elementHeight+conservativeHalfHeight then
                        [ ("transform","translateY(50px)")
                        , ("transition","translate 200ms ease-in-out")
                        ]
                    else if (myIdx>d.idx) && (d.currentY-d.startY)>(myIdx-d.idx)*elementHeight-conservativeHalfHeight then
                        [ ("transform","translateY(-50px)")
                        , ("transition","translate 200ms ease-in-out")
                        ]
                    else if (myIdx /= d.idx) then
                        [ ("transition","translate 200ms ease-in-out") ]
                    else
                        []
                Nothing ->
                        []
        changingStyle =
            case drag of
                Just _ ->
                    [ ("will-change","transform") ]
                Nothing ->
                    []
        constantStyle =
            [ ("box-sizing","border-box")
            , ("margin","0")
            , ("padding","0")
            , ("height",toString elementHeight ++ "px")
            ]
    in
        Html.tr [] 
            [ Html.td [] [Html.text "TODO: THIS"]
            , Html.td
                [ Html.Attributes.style <| makingWayStyle ++ moveStyle ++ changingStyle ++ constantStyle]
                [ Html.button 
                    [ Html.Attributes.style buttonStyle
                    , onMouseDown <| DragStart myIdx
                    ]
                    [Html.text ":::"]
                , Html.div 
                    [Html.Attributes.style [("display","inline-block")]] 
                    [item |> toString |> Html.text]
                ]
            ]



view : Model a -> Html Msg
view = viewWithEnable True

elementHeight : Int
elementHeight = 50

conservativeHalfHeight : Int
conservativeHalfHeight = 20

onMouseDown : (Int -> msg) -> Attribute msg
onMouseDown msg =
    Html.Events.on "mousedown" (Json.Decode.map (.y>>msg) Mouse.position)