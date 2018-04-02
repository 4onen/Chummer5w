module PointBy exposing (..)

type PointBy attributearraytype
    = Point Int (Maybe attributearraytype) (PointBy attributearraytype)
    | ZeroPoints

pointBy : Int -> PointBy attributearraytype
pointBy numPoints =
    Point numPoints Nothing (pointBy <| numPoints-1)

spendPoint : attributearraytype -> PointBy attributearraytype -> Result String (PointBy attributearraytype)
spendPoint pointClass points = 
    case points of
        ZeroPoints ->
            Result.Err "No points left to spend!"
        Point numRemaining Nothing remainingPoints ->
            Result.Ok <| Point numRemaining (Just pointClass) remainingPoints
        Point _ (Just _) remainingPoints ->
            spendPoint pointClass remainingPoints

unSpendPoint : attributearraytype -> PointBy attributearraytype -> Result String (PointBy attributearraytype)
unSpendPoint pointClass points =
    case points of
        ZeroPoints ->
            Result.Err "Yeah, you don't have any to un-spend."
        Point numRemaining (Just pointClass) remainingPoints ->
            Result.Ok <| Point numRemaining Nothing remainingPoints
        Point _ _ remainingPoints ->
            unSpendPoint pointClass remainingPoints