module PointBy exposing (..)

type PointBy attributearraytype
    = Point Int (Maybe attributearraytype) (PointBy attributearraytype)
    | ZeroPoints

pointBy : Int -> PointBy attributearraytype
pointBy numPoints =
    if numPoints > 0 then
        Point numPoints Nothing (pointBy (numPoints-1))
    else
        ZeroPoints


spendPoint : attributearraytype -> PointBy attributearraytype -> Result String (PointBy attributearraytype)
spendPoint pointClass points = 
    case points of
        ZeroPoints ->
            Result.Err "No points left to spend!"
        Point numRemaining Nothing remainingPoints ->
            Result.Ok <| Point numRemaining (Just pointClass) remainingPoints
        Point num (Just v) remainingPoints ->
            Result.map (Point num (Just v)) (spendPoint pointClass remainingPoints)


unspendPoint : attributearraytype -> PointBy attributearraytype -> Result String (PointBy attributearraytype)
unspendPoint pointClass points =
    case points of
        ZeroPoints ->
            Result.Err "Yeah, you don't have any to un-spend."
        Point numRemaining (Just pointClass) remainingPoints ->
            Result.Ok <| Point numRemaining Nothing remainingPoints
        Point num (v) remainingPoints ->
            Result.map (Point num (v)) (unspendPoint pointClass remainingPoints)


foldr : (attributearraytype -> acc -> acc) -> acc -> PointBy attributearraytype -> acc
foldr func acc points =
    case points of
        ZeroPoints ->
            acc
        Point _ (Just pointClass) remainingPoints ->
            func pointClass (foldr (func) acc remainingPoints)
        Point _ _ remainingPoints ->
            foldr (func) acc remainingPoints


foldl : (attributearraytype -> acc -> acc) -> acc -> PointBy attributearraytype -> acc
foldl func acc points =
    case points of
        ZeroPoints ->
            acc
        Point _ (Just pointClass) remainingPoints ->
            foldl func (func pointClass acc) remainingPoints
        Point _ _ remainingPoints ->
            foldl func acc remainingPoints


countPoints : attributearraytype -> PointBy attributearraytype -> Int
countPoints pointClass points =
    let
        check = (\point acc -> if point==pointClass then acc+1 else acc)
    in
        foldl check 0 points


countRemainingPoints : PointBy attributearraytype -> Int
countRemainingPoints points =
    case points of
        ZeroPoints -> 
            0
        Point _ (Nothing) remainingPoints ->
            (+) 1 <| countRemainingPoints remainingPoints
        Point _ _ remainingPoints ->
            countRemainingPoints remainingPoints
