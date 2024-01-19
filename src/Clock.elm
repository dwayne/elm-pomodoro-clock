module Clock exposing
    ( Clock
    , PhaseDuration(..)
    , State
    , decrement
    , decrementBreakLength
    , decrementSessionLength
    , incrementBreakLength
    , incrementSessionLength
    , init
    , isZero
    , switchPhase
    , toState
    )

import Duration exposing (Duration)
import Minutes exposing (Minutes)


type Clock
    = Clock State


type alias State =
    { breakLength : Minutes
    , sessionLength : Minutes
    , phaseDuration : PhaseDuration
    }


type PhaseDuration
    = Session Duration
    | Break Duration


init : { breakLength : Minutes, sessionLength : Minutes } -> Clock
init { breakLength, sessionLength } =
    Clock
        { breakLength = breakLength
        , sessionLength = sessionLength
        , phaseDuration = Session <| Minutes.toDuration sessionLength
        }


decrementBreakLength : Clock -> Clock
decrementBreakLength (Clock state) =
    let
        breakLength =
            Minutes.decrement state.breakLength
    in
    case state.phaseDuration of
        Session _ ->
            Clock { state | breakLength = breakLength }

        Break _ ->
            Clock
                { state
                    | breakLength = breakLength
                    , phaseDuration = Break <| Minutes.toDuration breakLength
                }


incrementBreakLength : Clock -> Clock
incrementBreakLength (Clock state) =
    let
        breakLength =
            Minutes.increment state.breakLength
    in
    case state.phaseDuration of
        Session _ ->
            Clock { state | breakLength = breakLength }

        Break _ ->
            Clock
                { state
                    | breakLength = breakLength
                    , phaseDuration = Break <| Minutes.toDuration breakLength
                }


decrementSessionLength : Clock -> Clock
decrementSessionLength (Clock state) =
    let
        sessionLength =
            Minutes.decrement state.sessionLength
    in
    case state.phaseDuration of
        Session _ ->
            Clock
                { state
                    | sessionLength = sessionLength
                    , phaseDuration = Session <| Minutes.toDuration sessionLength
                }

        Break _ ->
            Clock { state | sessionLength = sessionLength }


incrementSessionLength : Clock -> Clock
incrementSessionLength (Clock state) =
    let
        sessionLength =
            Minutes.increment state.sessionLength
    in
    case state.phaseDuration of
        Session _ ->
            Clock
                { state
                    | sessionLength = sessionLength
                    , phaseDuration = Session <| Minutes.toDuration sessionLength
                }

        Break _ ->
            Clock { state | sessionLength = sessionLength }


isZero : Clock -> Bool
isZero (Clock { phaseDuration }) =
    case phaseDuration of
        Session duration ->
            Duration.isZero duration

        Break duration ->
            Duration.isZero duration


decrement : Clock -> Clock
decrement (Clock state) =
    case state.phaseDuration of
        Session duration ->
            Clock { state | phaseDuration = Session <| Duration.decrement duration }

        Break duration ->
            Clock { state | phaseDuration = Break <| Duration.decrement duration }


switchPhase : Clock -> Clock
switchPhase (Clock state) =
    case state.phaseDuration of
        Session _ ->
            Clock { state | phaseDuration = Break <| Minutes.toDuration state.breakLength }

        Break _ ->
            Clock { state | phaseDuration = Session <| Minutes.toDuration state.sessionLength }


toState : Clock -> State
toState (Clock state) =
    state
