module Main exposing (main)


import Browser
import Html exposing (Html, a, button, div, footer, h1, h2, h3, i, span, text)
import Html.Attributes exposing (class, classList, disabled, href, target)
import Html.Events exposing (onClick)
import Time


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { break : Int
  , session : Int
  , timeLeft : Int
  , isRunning : Bool
  }


init : () -> (Model, Cmd msg)
init _ =
  ( defaultModel
  , Cmd.none
  )


defaultModel : Model
defaultModel =
  { break = 5
  , session = 25
  , timeLeft = 25 * 60
  , isRunning = False
  }


-- UPDATE


type Msg
  = ClickedDown Setting
  | ClickedUp Setting
  | ClickedReset
  | ClickedPlayPause
  | Tick Time.Posix


type Setting
  = Break
  | Session


update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    ClickedDown Break ->
      ( { model | break = decrement model.break }
      , Cmd.none
      )

    ClickedUp Break ->
      ( { model | break = increment model.break }
      , Cmd.none
      )

    ClickedDown Session ->
      let
        newSession =
          decrement model.session
      in
        ( { model | session = newSession, timeLeft = newSession * 60 }
        , Cmd.none
        )

    ClickedUp Session ->
      let
        newSession =
          increment model.session
      in
        ( { model | session = newSession, timeLeft = newSession * 60 }
        , Cmd.none
        )

    ClickedReset ->
      ( defaultModel
      , Cmd.none
      )

    ClickedPlayPause ->
      ( { model | isRunning = not model.isRunning }
      , Cmd.none
      )

    Tick _ ->
      ( if model.timeLeft > 0 then
          { model | timeLeft = model.timeLeft - 1 }
        else
          { model | timeLeft = model.session * 60, isRunning = False }
      , Cmd.none
      )


increment : Int -> Int
increment n =
  if n < 60 then
    n + 1
  else
    n


decrement : Int -> Int
decrement n =
  if n > 1 then
    n - 1
  else
    n


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { isRunning } =
  if isRunning then
    Time.every 1000 Tick
  else
    Sub.none


-- VIEW


view : Model -> Html Msg
view { break, session, timeLeft, isRunning } =
  div []
    [ h1 [] [ text "Pomodoro Clock" ]
    , div [ class "flex" ]
        [ viewSetting "Break Length" break Break isRunning
        , viewSetting "Session Length" session Session isRunning
        ]
    , viewTimer timeLeft
    , viewControls
    , viewAttribution
    ]


viewSetting : String -> Int -> Setting -> Bool -> Html Msg
viewSetting title value setting isDisabled =
  div [ class "setting" ]
    [ h2 [] [ text title ]
    , div []
        [ button
            [ class "button"
            , disabled isDisabled
            , onClick (ClickedDown setting)
            ]
            [ i [ class "fa fa-arrow-down fa-2x" ] [] ]
        , span [] [ text (String.fromInt value) ]
        , button
            [ class "button"
            , disabled isDisabled
            , onClick (ClickedUp setting)
            ]
            [ i [ class "fa fa-arrow-up fa-2x" ] [] ]
        ]
    ]


viewTimer : Int -> Html msg
viewTimer value =
  div
    [ class "mt timer"
    , classList
        [ ("is-expiring", value < 60)
        ]
    ]
    [ h3 [ class "timer__title" ] [ text "Session" ]
    , div [ class "timer__value" ] [ text (fromSeconds value) ]
    ]


viewControls : Html Msg
viewControls =
  div [ class "mt" ]
    [ button
        [ class "button"
        , onClick ClickedPlayPause
        ]
        [ i [ class "fa fa-play fa-2x" ] []
        , i [ class "fa fa-pause fa-2x" ] []
        ]
    , button
        [ class "button"
        , onClick ClickedReset
        ]
        [ i [ class "fa fa-refresh fa-2x" ] [] ]
    ]


viewAttribution : Html msg
viewAttribution =
  footer [ class "mt attribution" ]
    [ text "Developed by"
    , a
      [ class "developer"
      , href "https://github.com/dwayne/"
      , target "_blank"
      ]
      [ text "Dwayne Crooks" ]
    ]


-- HELPERS


fromSeconds : Int -> String
fromSeconds total =
  let
    mins =
      total // 60

    secs =
      modBy 60 total
  in
    String.concat
      [ String.padLeft 2 '0' (String.fromInt mins)
      , ":"
      , String.padLeft 2 '0' (String.fromInt secs)
      ]
