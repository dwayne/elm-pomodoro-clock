module Main exposing (main)


import Browser
import Html exposing (Html, a, button, div, footer, h1, h2, h3, i, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


-- MODEL


type alias Model =
  { break : Int
  , session : Int
  , timeLeft : Int
  }


init : Model
init =
  { break = 5
  , session = 25
  , timeLeft = 25 * 60
  }


-- UPDATE


type Msg
  = ClickedDown Setting
  | ClickedUp Setting
  | ClickedReset


type Setting
  = Break
  | Session


update : Msg -> Model -> Model
update msg model =
  case msg of
    ClickedDown Break ->
      { model | break = decrement model.break }

    ClickedUp Break ->
      { model | break = increment model.break }

    ClickedDown Session ->
      { model | session = decrement model.session }

    ClickedUp Session ->
      { model | session = increment model.session }

    ClickedReset ->
      init


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


-- VIEW


view : Model -> Html Msg
view { break, session, timeLeft } =
  div []
    [ h1 [] [ text "Pomodoro Clock" ]
    , div [ class "flex" ]
        [ viewSetting "Break Length" break Break
        , viewSetting "Session Length" session Session
        ]
    , viewTimer timeLeft
    , viewControls
    , viewAttribution
    ]


viewSetting : String -> Int -> Setting -> Html Msg
viewSetting title value setting =
  div [ class "setting" ]
    [ h2 [] [ text title ]
    , div []
        [ button
            [ class "button"
            , onClick (ClickedDown setting)
            ]
            [ i [ class "fa fa-arrow-down fa-2x" ] [] ]
        , span [] [ text (String.fromInt value) ]
        , button
            [ class "button"
            , onClick (ClickedUp setting)
            ]
            [ i [ class "fa fa-arrow-up fa-2x" ] [] ]
        ]
    ]


viewTimer : Int -> Html msg
viewTimer value =
  div [ class "mt timer" ]
    [ h3 [ class "timer__title" ] [ text "Session" ]
    , div [ class "timer__value" ] [ text (fromSeconds value) ]
    ]


viewControls : Html Msg
viewControls =
  div [ class "mt" ]
    [ button [ class "button" ]
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
