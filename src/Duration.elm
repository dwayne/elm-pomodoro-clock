module Duration exposing
  ( Duration
  , fromSeconds
  , isZero
  , isLessThanOneMinute
  , decrement
  , toString
  )


type Duration
  = Duration Int
-- min duration = 0 secs
-- max duration = 1 hour = 60 mins = 3600 secs


fromSeconds : Int -> Duration
fromSeconds =
  Duration << clamp 0 3600


isZero : Duration -> Bool
isZero (Duration seconds) =
  seconds == 0


isLessThanOneMinute : Duration -> Bool
isLessThanOneMinute (Duration seconds) =
  seconds < 60


decrement : Duration -> Duration
decrement (Duration seconds) =
  Duration <| seconds - 1


toString : Duration -> String
toString (Duration seconds) =
  let
    (mins, secs) =
      ( seconds // 60
      , modBy 60 seconds
      )
  in
  toDDString mins ++ ":" ++ toDDString secs


toDDString : Int -> String
toDDString =
  String.padLeft 2 '0' << String.fromInt
-- toDDString 0 = "00"
-- toDDString 60 = "60"
