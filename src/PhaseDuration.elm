module PhaseDuration exposing
    ( PhaseDuration(..)
    , decrement
    , isZero
    )

import Duration exposing (Duration)


type PhaseDuration
    = Session Duration
    | Break Duration


isZero : PhaseDuration -> Bool
isZero phaseDuration =
    case phaseDuration of
        Session duration ->
            Duration.isZero duration

        Break duration ->
            Duration.isZero duration


decrement : PhaseDuration -> PhaseDuration
decrement phaseDuration =
    case phaseDuration of
        Session duration ->
            Session <| Duration.decrement duration

        Break duration ->
            Break <| Duration.decrement duration
