port module Main exposing (main)


import Browser
import Html exposing (Html, a, audio, button, div, footer, h1, h2, h3, i, span, text)
import Html.Attributes exposing (class, classList, disabled, href, id, src, target)
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
  , state : State
  }


type State
  = Paused Phase
  | Running Phase


type Phase
  = Session
  | Break


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
  , state = Paused Session
  }


-- UPDATE


type Msg
  = ClickedDownBreak
  | ClickedUpBreak
  | ClickedDownSession
  | ClickedUpSession
  | ClickedReset
  | ClickedPlayPause
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case model.state of
    Running phase ->
      case msg of
        ClickedReset ->
          ( defaultModel
          , Cmd.none
          )

        ClickedPlayPause ->
          ( { model | state = Paused phase }
          , Cmd.none
          )

        Tick _ ->
          if model.timeLeft > 0 then
            ( { model | timeLeft = model.timeLeft - 1 }
            , Cmd.none
            )
          else
            let
              newModel =
                case phase of
                  Session ->
                    { model | timeLeft = model.break * 60, state = Running Break }

                  Break ->
                    { model | timeLeft = model.session * 60, state = Running Session }
            in
              ( newModel
              , play ()
              )

        _ ->
          ( model
          , Cmd.none
          )

    Paused phase ->
      case msg of
        ClickedDownBreak ->
          let
            newBreak =
              decrement model.break
          in
            ( case phase of
                Session ->
                  { model | break = newBreak }

                Break ->
                  { model | break = newBreak, timeLeft = newBreak * 60 }
            , Cmd.none
            )

        ClickedUpBreak ->
          let
            newBreak =
              increment model.break
          in
            ( case phase of
                Session ->
                  { model | break = newBreak }

                Break ->
                  { model | break = newBreak, timeLeft = newBreak * 60 }
            , Cmd.none
            )

        ClickedDownSession ->
          let
            newSession =
              decrement model.session
          in
            ( case phase of
                Session ->
                  { model | session = newSession, timeLeft = newSession * 60 }

                Break ->
                  { model | session = newSession }
            , Cmd.none
            )

        ClickedUpSession ->
          let
            newSession =
              increment model.session
          in
            ( case phase of
                Session ->
                  { model | session = newSession, timeLeft = newSession * 60 }

                Break ->
                  { model | session = newSession }
            , Cmd.none
            )

        ClickedReset ->
          ( defaultModel
          , Cmd.none
          )

        ClickedPlayPause ->
          ( { model | state = Running phase }
          , Cmd.none
          )

        _ ->
          ( model
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


-- COMMANDS


port play : () -> Cmd msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { state } =
  if isRunning state then
    Time.every 1000 Tick
  else
    Sub.none


-- VIEW


view : Model -> Html Msg
view { break, session, timeLeft, state } =
  let
    isDisabled =
      isRunning state
  in
    div []
      [ h1 [] [ text "25 + 5 Clock" ]
      , div [ class "flex" ]
          [ viewSetting "Break Length" break isDisabled ClickedDownBreak ClickedUpBreak
          , viewSetting "Session Length" session isDisabled ClickedDownSession ClickedUpSession
          ]
      , viewTimer timeLeft state
      , viewControls
      , viewAttribution
      ]


viewSetting : String -> Int -> Bool -> msg -> msg -> Html msg
viewSetting title value isDisabled down up =
  div [ class "setting" ]
    [ h2 [] [ text title ]
    , div []
        [ button
            [ class "button"
            , disabled isDisabled
            , onClick down
            ]
            [ i [ class "fa fa-arrow-down fa-2x" ] [] ]
        , span [] [ text (String.fromInt value) ]
        , button
            [ class "button"
            , disabled isDisabled
            , onClick up
            ]
            [ i [ class "fa fa-arrow-up fa-2x" ] [] ]
        ]
    ]


viewTimer : Int -> State -> Html msg
viewTimer value state =
  let
    title =
      case currentPhase state of
        Session ->
          "Session"

        Break ->
          "Break"
  in
    div
      [ class "mt timer", classList [ ("is-expiring", value < 60) ] ]
      [ h3 [ class "timer__title" ] [ text title ]
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


isRunning : State -> Bool
isRunning state =
  case state of
    Paused _ ->
      False

    Running _ ->
      True


currentPhase : State -> Phase
currentPhase state =
  case state of
    Paused phase ->
      phase

    Running phase ->
      phase
