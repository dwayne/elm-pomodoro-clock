module PhaseDuration exposing
  ( PhaseDuration(..)
  , isZero
  , decrement
  , toString
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


toString : PhaseDuration -> String
toString phaseDuration =
  case phaseDuration of
    Session duration ->
      Duration.toString duration

    Break duration ->
      Duration.toString duration
