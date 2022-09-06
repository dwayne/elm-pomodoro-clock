module Minutes exposing
  ( Minutes
  , fromInt
  , decrement, increment
  , toString, toDuration
  )


import Duration exposing (Duration)


type Minutes =
  Minutes Int
-- min minutes = 1 min
-- max minutes = 60 mins


fromInt : Int -> Minutes
fromInt =
  Minutes << clamp 1 60


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
