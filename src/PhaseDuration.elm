module PhaseDuration exposing (PhaseDuration(..), toString)


import Duration exposing (Duration)


type PhaseDuration
  = Session Duration
  | Break Duration


toString : PhaseDuration -> String
toString phaseDuration =
  case phaseDuration of
    Session duration ->
      Duration.toString duration

    Break duration ->
      Duration.toString duration
