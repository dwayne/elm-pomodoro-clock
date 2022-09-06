port module Main exposing (main)


import Browser
import Duration exposing (Duration)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Minutes exposing (Minutes)
import PhaseDuration exposing (PhaseDuration)
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
  { isPlaying : Bool
  , breakLength : Minutes
  , sessionLength : Minutes
  , phaseDuration : PhaseDuration
  }


init : () -> (Model, Cmd msg)
init _ =
  let
    sessionLength =
      Minutes.fromInt 25
  in
  ( { isPlaying = False
    , breakLength = Minutes.fromInt 5
    , sessionLength = sessionLength
    , phaseDuration = PhaseDuration.Session <| Minutes.toDuration sessionLength
    }
  , Cmd.none
  )


-- UPDATE


type Msg
  = DecrementedBreakLength
  | IncrementedBreakLength
  | DecrementedSessionLength
  | IncrementedSessionLength
  | ClickedPlayPause
  | ClickedRefresh
  | Tick


update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  if model.isPlaying then
    updatePlaying msg model
  else
    updatePaused msg model


updatePaused : Msg -> Model -> (Model, Cmd msg)
updatePaused msg model =
  case msg of
    DecrementedBreakLength ->
      ( updateBreakLength (Minutes.decrement model.breakLength) model
      , Cmd.none
      )

    IncrementedBreakLength ->
      ( updateBreakLength (Minutes.increment model.breakLength) model
      , Cmd.none
      )

    DecrementedSessionLength ->
      ( updateSessionLength (Minutes.decrement model.sessionLength) model
      , Cmd.none
      )

    IncrementedSessionLength ->
      ( updateSessionLength (Minutes.increment model.sessionLength) model
      , Cmd.none
      )

    ClickedPlayPause ->
      togglePlayPause model

    ClickedRefresh ->
      refresh

    Tick ->
      noop model


updatePlaying : Msg -> Model -> (Model, Cmd msg)
updatePlaying msg model =
  case msg of
    DecrementedBreakLength ->
      noop model

    IncrementedBreakLength ->
      noop model

    DecrementedSessionLength ->
      noop model

    IncrementedSessionLength ->
      noop model

    ClickedPlayPause ->
      togglePlayPause model

    ClickedRefresh ->
      refresh

    Tick ->
      if PhaseDuration.isZero model.phaseDuration then
        ( updateAndSwitchPhaseDuration model
        , play ()
        )
      else
        ( { model | phaseDuration = PhaseDuration.decrement model.phaseDuration }
        , Cmd.none
        )


updateBreakLength : Minutes -> Model -> Model
updateBreakLength breakLength model =
  case model.phaseDuration of
    PhaseDuration.Break _ ->
      { model
      | breakLength = breakLength
      , phaseDuration = PhaseDuration.Break <| Minutes.toDuration breakLength
      }

    PhaseDuration.Session _ ->
      { model | breakLength = breakLength }


updateSessionLength : Minutes -> Model -> Model
updateSessionLength sessionLength model =
  case model.phaseDuration of
    PhaseDuration.Session _ ->
      { model
      | sessionLength = sessionLength
      , phaseDuration = PhaseDuration.Session <| Minutes.toDuration sessionLength
      }

    PhaseDuration.Break _ ->
      { model | sessionLength = sessionLength }


updateAndSwitchPhaseDuration : Model -> Model
updateAndSwitchPhaseDuration model =
  case model.phaseDuration of
    PhaseDuration.Session _ ->
      { model
      | phaseDuration =
          PhaseDuration.Break <| Minutes.toDuration model.breakLength
      }

    PhaseDuration.Break _ ->
      { model
      | phaseDuration =
          PhaseDuration.Session <| Minutes.toDuration model.sessionLength
      }


togglePlayPause : Model -> (Model, Cmd msg)
togglePlayPause model =
  ( { model | isPlaying = not model.isPlaying }
  , Cmd.none
  )


refresh : (Model, Cmd msg)
refresh =
  init ()


noop : Model -> (Model, Cmd msg)
noop model =
  ( model
  , Cmd.none
  )


-- PORTS


port play : () -> Cmd msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.isPlaying then
    Time.every 1000 <| always Tick
  else
    Sub.none


-- VIEW


view : Model -> H.Html Msg
view { breakLength, sessionLength, phaseDuration } =
  viewLayout <|
    viewMain
      { clock =
          { title = "25 + 5 Clock"
          , break =
              { title = "Break Length"
              , minutes = breakLength
              , onDecrement = DecrementedBreakLength
              , onIncrement = IncrementedBreakLength
              }
          , session =
              { title = "Session Length"
              , minutes = sessionLength
              , onDecrement = DecrementedSessionLength
              , onIncrement = IncrementedSessionLength
              }
          , display =
              case phaseDuration of
                PhaseDuration.Session duration ->
                  { title = "Session"
                  , duration = duration
                  }

                PhaseDuration.Break duration ->
                  { title = "Break"
                  , duration = duration
                  }
          , onPlayPause = ClickedPlayPause
          , onRefresh = ClickedRefresh
          }
      , attribution =
          { name = "Dwayne Crooks"
          , url = "https://github.com/dwayne"
          }
      }


viewLayout : H.Html msg -> H.Html msg
viewLayout content =
  H.div [ HA.class "layout" ]
    [ H.div [ HA.class "layout__wrapper" ]
        [ H.div [ HA.class "layout__main" ] [ content ] ]
    ]


viewMain :
  { clock : Clock msg
  , attribution : Attribution
  }
  -> H.Html msg
viewMain { clock, attribution } =
  H.main_ [ HA.class "main" ]
    [ H.div [ HA.class "main__clock" ] [ viewClock clock ]
    , H.footer [ HA.class "main__attribution" ] [ viewAttribution attribution ]
    ]


type alias Clock msg =
  { title : String
  , break : Setting msg
  , session : Setting msg
  , display : Display
  , onPlayPause : msg
  , onRefresh : msg
  }


viewClock : Clock msg -> H.Html msg
viewClock { title, break, session, display, onPlayPause, onRefresh } =
  H.div [ HA.class "clock" ]
    [ H.div [ HA.class "clock__title" ] [ viewTitle title ]
    , H.div [ HA.class "clock__settings" ]
        [ H.div [ HA.class "clock__break-setting" ] [ viewSetting break ]
        , H.div [ HA.class "clock__session-setting" ] [ viewSetting session ]
        ]
    , H.div [ HA.class "clock__display" ] [ viewDisplay display ]
    , H.div [ HA.class "clock__controls" ]
        [ H.div [ HA.class "clock__play-pause-button" ]
            [ viewButton <| PlayPause onPlayPause ]
        , H.div [ HA.class "clock__refresh-button" ]
            [ viewButton <| Refresh onRefresh ]
        ]
    ]


viewTitle : String -> H.Html msg
viewTitle title =
  H.h1 [] [ H.text title ]


type alias Setting msg =
  { title : String
  , minutes : Minutes
  , onDecrement : msg
  , onIncrement : msg
  }


viewSetting : Setting msg -> H.Html msg
viewSetting { title, minutes, onDecrement, onIncrement } =
  H.div [ HA.class "setting" ]
    [ H.h2 [ HA.class "setting__title" ] [ H.text title ]
    , H.div [ HA.class "setting__controls" ]
        [ H.div [ HA.class "setting__button" ]
            [ viewButton <| ArrowDown onDecrement ]
        , H.span [ HA.class "setting__value" ]
            [ H.text <| Minutes.toString minutes ]
        , H.div [ HA.class "setting__button" ]
            [ viewButton <| ArrowUp onIncrement ]
        ]
    ]


type Button msg
  = ArrowDown msg
  | ArrowUp msg
  | PlayPause msg
  | Refresh msg


viewButton : Button msg -> H.Html msg
viewButton button =
  case button of
    ArrowDown onDecrement ->
      H.button
        [ HA.class "button"
        , HE.onClick onDecrement
        ]
        [ H.i [ HA.class "fa fa-arrow-down fa-2x" ] [] ]

    ArrowUp onIncrement ->
      H.button
        [ HA.class "button"
        , HE.onClick onIncrement
        ]
        [ H.i [ HA.class "fa fa-arrow-up fa-2x" ] [] ]

    PlayPause onPlayPause ->
      H.button
        [ HA.class "button"
        , HE.onClick onPlayPause
        ]
        [ H.i [ HA.class "fa fa-play fa-2x" ] []
        , H.i [ HA.class "fa fa-pause fa-2x" ] []
        ]

    Refresh onRefresh ->
      H.button
        [ HA.class "button"
        , HE.onClick onRefresh
        ]
        [ H.i [ HA.class "fa fa-refresh fa-2x" ] [] ]


type alias Display =
  { title : String
  , duration : Duration
  }


viewDisplay : Display -> H.Html msg
viewDisplay { title, duration } =
  H.div
    [ HA.class "display"
    , HA.classList
        [ ( "display--warning", Duration.isLessThanOneMinute duration )
        ]
    ]
    [ H.h3 [ HA.class "display__title" ] [ H.text title ]
    , H.div [ HA.class "display__value" ]
        [ H.text <| Duration.toString duration ]
    ]


type alias Attribution =
  { name : String
  , url : String
  }


viewAttribution : Attribution -> H.Html msg
viewAttribution { name, url } =
  H.p [ HA.class "attribution" ]
    [ H.text "Developed by "
    , H.a
        [ HA.href url
        , HA.target "_blank"
        , HA.class "attribution__link"
        ]
        [ H.text name ]
    ]
