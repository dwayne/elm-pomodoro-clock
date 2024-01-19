module Minutes exposing
    ( Minutes
    , decrement
    , fromInt
    , increment
    , isMax
    , isMin
    , toDuration
    , toString
    )

import Duration exposing (Duration)


type
    Minutes
    --
    -- Minimum: 1 min
    -- Maximum: 60 mins
    --
    = Minutes Int


fromInt : Int -> Minutes
fromInt =
    Minutes << clamp 1 60


isMin : Minutes -> Bool
isMin (Minutes mins) =
    mins == 1


isMax : Minutes -> Bool
isMax (Minutes mins) =
    mins == 60


decrement : Minutes -> Minutes
decrement (Minutes mins) =
    fromInt <| mins - 1


increment : Minutes -> Minutes
increment (Minutes mins) =
    fromInt <| mins + 1


toString : Minutes -> String
toString (Minutes mins) =
    String.fromInt mins


toDuration : Minutes -> Duration
toDuration (Minutes mins) =
    Duration.fromSeconds <| mins * 60
