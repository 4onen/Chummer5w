module Priorities exposing (..)

type Priority 
    = Heritage
    | Talent
    | Attributes
    | Skills
    | Resources

--Priorities is a number between 0 and 120,
-- because 5*4*3*2*1 = 120 states of the list
-- of priorities. 
type alias Priorities =
    Int

toList : Priorities -> List Priority
toList priorities =
    let
        selectFrom5 = [Heritage,Talent,Attributes,Skills,Resources]
        (v1,selectFrom4) = 
            selectFrom5
                |> List.indexedMap (\i v -> ((priorities//1)%5==i,v))
                |> List.partition (\(b,_) -> b)
                |> Tuple.mapFirst (\l -> l |> List.unzip |> Tuple.second |> List.head)
                |> Tuple.mapSecond (\l -> l |> List.unzip |> Tuple.second)
        (v2,selectFrom3) = 
            selectFrom4
                |> List.indexedMap (\i v -> ((priorities//5)%4==i,v))
                |> List.partition (\(b,_) -> b)
                |> Tuple.mapFirst (\l -> l |> List.unzip |> Tuple.second |> List.head)
                |> Tuple.mapSecond (\l -> l |> List.unzip |> Tuple.second)
        (v3,selectFrom2) = 
            selectFrom3
                |> List.indexedMap (\i v -> ((priorities//20)%3==i,v))
                |> List.partition (\(b,_) -> b)
                |> Tuple.mapFirst (\l -> l |> List.unzip |> Tuple.second |> List.head)
                |> Tuple.mapSecond (\l -> l |> List.unzip |> Tuple.second)
        v45 = 
            if priorities//60%2==0 
            then selectFrom2 
            else List.reverse selectFrom2
    in
        case ([v1,v2,v3],v45) of
            ([Just realv1, Just realv2, Just realv3],[realv4,realv5]) ->
                [realv1,realv2,realv3]++v45
            _ ->
                selectFrom5

raise : Priority -> Priorities -> Priorities
raise priority priorities =
    let
        pnum1 =
            case priority of
                Heritage -> 0
                Talent -> 1
                Attributes -> 2
                Skills -> 3
                Resources -> 4
        select1 = priorities%5
        pnum2 = if select1<pnum1 then pnum1-1 else pnum1
        select2 = (priorities//5)%4
        pnum3 = if select2<pnum2 then pnum2-1 else pnum2
        select3 = (priorities//20)%3
        pnum4 = if select3<pnum3 then pnum3-1 else pnum3
        select4 = (priorities//60)%2
    in
        case (pnum1==select1,pnum2==select2,pnum3==select3,pnum4==select4) of
            (True,_,_,_) ->
                priorities
            (_,True,_,_) ->
                priorities+1
            (_,_,True,_) ->
                priorities+5
            (_,_,_,True) ->
                priorities+20
            _ ->
                priorities+60