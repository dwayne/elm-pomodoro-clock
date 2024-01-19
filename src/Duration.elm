module Duration exposing
    ( Duration
    , decrement
    , fromSeconds
    , isLessThanOneMinute
    , isZero
    , toString
    )


type
    Duration
    --
    -- Minimum: 0 secs
    -- Maximum: 1 hour = 60 mins = 3600 secs
    --
    = Duration Int


fromSeconds : Int -> Duration
fromSeconds =
    Duration << clamp 0 3600


isZero : Duration -> Bool
isZero (Duration seconds) =
    seconds == 0


isLessThanOneMinute : Duration -> Bool
isLessThanOneMinute (Duration seconds) =
    --
    -- 1 min = 60 secs
    --
    seconds < 60


decrement : Duration -> Duration
decrement (Duration seconds) =
    fromSeconds <| seconds - 1


toString : Duration -> String
toString (Duration seconds) =
    let
        ( mins, secs ) =
            ( seconds // 60
            , modBy 60 seconds
            )
    in
    toDDString mins ++ ":" ++ toDDString secs


toDDString : Int -> String
toDDString =
    --
    -- 0  -> "00"
    -- 1  -> "01"
    -- ...
    -- 9  -> "09"
    -- 10 -> "10"
    -- ...
    -- 60 -> "60"
    --
    String.padLeft 2 '0' << String.fromInt
