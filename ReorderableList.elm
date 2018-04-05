module ReorderableList exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import Json.Decode
import Mouse

type alias Model val =
    { data : List val
    , drag : Maybe Drag
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
    , updownButtons : Maybe (Index -> val -> Bool -> Html msg)
    }

type alias Index = Int

init : Model val
init = fromList []

fromList : List val -> Model val
fromList input =
    {data = input, drag = Nothing}

toList : Model val -> List val
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
    | RaiseItem Int
    | LowerItem Int

updateList : (List val -> List val) -> Model val -> Model val
updateList func model =
    {model | drag = Nothing, data = func model.data}

update : Msg -> Model val -> Model val
update msg model =
    case msg of
        DragStart idx y ->
            {model | drag = Just (Drag idx y y)}
        DragAt y ->
            {model | drag = Maybe.map (updateDragAt y) model.drag}
        DragEnd y ->
            (updateDragEnd y model)
        RaiseItem idx ->
            {model | data = moveItem idx -1 model.data}
        LowerItem idx ->
            {model | data = moveItem idx 1 model.data}


updateDragAt : Int -> Drag -> Drag
updateDragAt y drag =
    {drag|currentY = y}


updateDragEnd : Int -> Model val -> Model val
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


subscriptions : Model val -> Sub Msg
subscriptions {drag} =
    case drag of
        Just _ -> Sub.batch [Mouse.moves (.y>>DragAt), Mouse.ups (.y>>DragEnd)]
        Nothing -> Sub.none


viewWithOptions : (Msg -> msg) -> Options val msg -> Model val -> Html msg
viewWithOptions msg opts model =
    Html.table 
        [ Html.Attributes.style 
            [ ("margin","0")
            , ("padding","0")
            , ("list-style-type","none") 
            , ("vertical-align","middle")
            ]
        ]
        (List.indexedMap (itemView msg opts model) model.data)


itemView : (Msg -> msg) -> Options val msg -> Model val -> Index -> val -> Html msg
itemView msg opts {data,drag} myIdx item =
    let
        buttonStyle = 
            if opts.enable then 
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
        updownButtons = 
            case opts.updownButtons of
                Just f ->
                    [ Html.button 
                        [ Html.Attributes.style buttonStyle
                        , Html.Attributes.disabled (myIdx==0)
                        , Html.Events.onClick <| msg (RaiseItem myIdx)
                        ] 
                        [f myIdx item True]
                    , Html.button 
                        [ Html.Attributes.style buttonStyle
                        , Html.Attributes.disabled (myIdx==(List.length data)-1)
                        , Html.Events.onClick <| msg (LowerItem myIdx)
                        ] 
                        [f myIdx item False]
                    ]
                Nothing -> 
                    []
    in
        Html.tr [] 
            [ Html.td [] [rowTag]
            , Html.td [] updownButtons
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



view : Model val -> Html Msg
view = viewWithOptions identity (Options True Nothing Nothing Nothing)

elementHeight : Int
elementHeight = 50

conservativeHalfHeight : Int
conservativeHalfHeight = 20

onMouseDown : (Int -> msg) -> Attribute msg
onMouseDown msg =
    Html.Events.on "mousedown" (Json.Decode.map (.y>>msg) Mouse.position)
