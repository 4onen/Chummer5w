module ReorderableList exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import Json.Decode
import Mouse

type alias Model =
    { drag : Maybe Drag
    }

type alias Drag =
    { idx : Index
    , startY : Int
    , currentY : Int
    }

type alias Options val msg =
    { enable : Bool
    , rowTagFunc : Maybe (Index -> Html msg)
    , labelFunc : Maybe (Index -> val -> Html msg)
    }

type alias Index = Int

init : Model
init = Model Nothing

type Msg 
    = DragStart Index Int
    | DragAt Int
    | DragEnd Int
    | KillDrag

update : Msg -> Model -> List val -> (Model, List val)
update msg model list =
    case msg of
        DragStart idx y ->
            ({model | drag = Just (Drag idx y y)}, list)
        DragAt y ->
            ({model | drag = Maybe.map (updateDragAt y) model.drag}, list)
        DragEnd y ->
            (updateDragEnd y model list)
        KillDrag ->
            ({model | drag = Nothing}, list)


killDrag : Model -> Model
killDrag =
    Tuple.first << ((flip (update KillDrag)) [])


updateDragAt : Int -> Drag -> Drag
updateDragAt y drag =
    {drag|currentY = y}


updateDragEnd : Int -> Model -> List val -> (Model,List val)
updateDragEnd y model list =
    case model.drag of
        Just d ->
            ( { model | drag = Nothing }
            , moveItem d.idx ((d.currentY - d.startY + (if d.currentY < d.startY then -conservativeHalfHeight else conservativeHalfHeight))//elementHeight) list)
        Nothing ->
            ( model, list )


moveItem : Index -> Index -> List a -> List a
moveItem from by oldList =
    let
        listWithoutMoved = (List.take from oldList) ++ (List.drop (from+1) oldList)
        moved = oldList |> List.drop from |> List.take 1
    in
        (List.take (from + by) listWithoutMoved)
            ++ moved
            ++ (List.drop (from + by) listWithoutMoved)


subscriptions : Model -> Sub Msg
subscriptions {drag} =
    case drag of
        Just _ -> Sub.batch [Mouse.moves (.y>>DragAt), Mouse.ups (.y>>DragEnd)]
        Nothing -> Sub.none


viewWithOptions : (Msg -> msg) -> Options val msg -> Model -> List val -> Html msg
viewWithOptions msg opts model list =
    Html.table 
        [ Html.Attributes.style 
            [ ("margin","0")
            , ("padding","0")
            , ("list-style-type","none") 
            , ("vertical-align","middle")
            ]
        ]
        (List.indexedMap (itemView msg opts model) list)


itemView : (Msg -> msg) -> Options val msg -> Model -> Index -> val -> Html msg
itemView msg opts {drag} myIdx item =
    let
        buttonStyle = 
            if opts.enable then 
                [("display","inline-block")] 
            else 
                [("display","none")]
        moveStyle = 
            drag
                |> Maybe.andThen
                    (\d -> 
                        if d.idx == myIdx then
                            Just 
                                [ ("transform","translateY("++toString (d.currentY-d.startY)++"px) translateZ(10px)")
                                , ("box-shadow","0 3px 6px rgba(0,0,0,0.24)")
                                , ("will-change","transform")
                                ]
                        else
                            Nothing
                    )
                |> Maybe.withDefault []
        makingWayStyle =
            case drag of
                Just d ->
                    if (myIdx<d.idx) && (d.currentY-d.startY)<(myIdx-d.idx)*elementHeight+conservativeHalfHeight then
                        [ ("transform","translateY(50px)") ]
                    else if (myIdx>d.idx) && (d.currentY-d.startY)>(myIdx-d.idx)*elementHeight-conservativeHalfHeight then
                        [ ("transform","translateY(-50px)") ]
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
        rowTag =
            case opts.rowTagFunc of
                Just f ->
                    f myIdx
                Nothing ->
                    Html.text ""
        label =
            case opts.labelFunc of
                Just f ->
                    f myIdx item
                Nothing ->
                    Html.text (toString item)
    in
        Html.tr [] 
            [ Html.td [] [rowTag]
            , Html.td
                [ Html.Attributes.style <| makingWayStyle ++ moveStyle ++ changingStyle ++ constantStyle]
                [ Html.button 
                    [ Html.Attributes.style buttonStyle
                    , onMouseDown <| (\i -> msg (DragStart myIdx i))
                    ]
                    [Html.text ":::"]
                , Html.div 
                    [Html.Attributes.style [("display","inline-block")]] 
                    [label]
                ]
            ]



view : Model -> List val -> Html Msg
view = viewWithOptions identity (Options True Nothing Nothing)

elementHeight : Int
elementHeight = 50

conservativeHalfHeight : Int
conservativeHalfHeight = 20

onMouseDown : (Int -> msg) -> Attribute msg
onMouseDown msg =
    Html.Events.on "mousedown" (Json.Decode.map (.y>>msg) Mouse.position)
