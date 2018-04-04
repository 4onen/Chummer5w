module Priorities exposing (..)

import Array exposing (Array)

type Priority 
    = Heritage
    | Talent
    | Attributes
    | Skills
    | Resources

type PriorityChar
    = APriority
    | BPriority
    | CPriority
    | DPriority
    | EPriority

--Priorities is a number between 0 and 120,
-- because 5*4*3*2*1 = 120 states of the list
-- of priorities. 
type Priorities = Priorities (Array Priority)

default : Priorities
default = Priorities <| Array.fromList [Heritage,Talent,Attributes,Skills,Resources]

swap : Int -> Int -> Priorities -> Priorities
swap i1 i2 (Priorities arr) =
    case (Array.get i1 arr, Array.get i2 arr) of
        (Just e1, Just e2) ->
            arr |> Array.set i1 e2
                |> Array.set i2 e1
                |> Priorities
        _ ->
            Priorities arr

raise : Int -> Priorities -> Priorities
raise i ps =
    swap (i) (i-1) ps

lower : Int -> Priorities -> Priorities
lower i ps =
    swap (i) (i+1) ps

getChar : Priority -> Priorities -> PriorityChar
getChar p (Priorities ps) =
    ps  |> Array.indexedMap (,)
        |> Array.foldl (\(i,v) acc -> if v==p then i else acc) -1
        |> (\i -> case i of 
            0 -> APriority
            1 -> BPriority
            2 -> CPriority
            3 -> DPriority
            4 -> EPriority
            _ -> EPriority
           )
