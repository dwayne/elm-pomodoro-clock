module Duration exposing
  ( Duration
  , fromSeconds
  , isLessThanOneMinute
  , toString
  )


type Duration
  = Duration Int
-- min duration = 0 secs
-- max duration = 1 hour = 60 mins = 3600 secs


fromSeconds : Int -> Duration
fromSeconds =
  Duration << clamp 0 3600


isLessThanOneMinute : Duration -> Bool
isLessThanOneMinute (Duration seconds) =
  seconds < 60


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
