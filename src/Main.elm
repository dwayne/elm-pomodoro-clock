module Main exposing (main)


import Browser
import Duration exposing (Duration)
import Html as H
import Html.Attributes as HA
import Minutes exposing (Minutes)
import PhaseDuration exposing (PhaseDuration)


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model =
  { breakLength : Minutes
  , sessionLength : Minutes
  , phaseDuration : PhaseDuration
  }


init : () -> (Model, Cmd msg)
init _ =
  let
    sessionLength =
      Minutes.fromInt 25
  in
  ( { breakLength = Minutes.fromInt 5
    , sessionLength = sessionLength
    , phaseDuration = PhaseDuration.Session <| Minutes.toDuration sessionLength
    }
  , Cmd.none
  )


-- UPDATE


type alias Msg = {}


update : Msg -> Model -> (Model, Cmd msg)
update _ model =
  ( model
  , Cmd.none
  )


-- VIEW


view : Model -> H.Html msg
view { breakLength, sessionLength, phaseDuration } =
  viewLayout <|
    viewMain
      { clock =
          { title = "25 + 5 Clock"
          , break =
              { title = "Break Length"
              , minutes = breakLength
              }
          , session =
              { title = "Session Length"
              , minutes = sessionLength
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
  { clock : Clock
  , attribution : Attribution
  }
  -> H.Html msg
viewMain { clock, attribution } =
  H.main_ [ HA.class "main" ]
    [ H.div [ HA.class "main__clock" ] [ viewClock clock ]
    , H.footer [ HA.class "main__attribution" ] [ viewAttribution attribution ]
    ]


type alias Clock =
  { title : String
  , break : Setting
  , session : Setting
  , display : Display
  }


viewClock : Clock -> H.Html msg
viewClock { title, break, session, display } =
  H.div [ HA.class "clock" ]
    [ H.div [ HA.class "clock__title" ] [ viewTitle title ]
    , H.div [ HA.class "clock__settings" ]
        [ H.div [ HA.class "clock__break-setting" ] [ viewSetting break ]
        , H.div [ HA.class "clock__session-setting" ] [ viewSetting session ]
        ]
    , H.div [ HA.class "clock__display" ] [ viewDisplay display ]
    , H.div [ HA.class "clock__controls" ]
        [ H.div [ HA.class "clock__play-pause-button" ]
            [ viewButton PlayPause ]
        , H.div [ HA.class "clock__refresh-button" ]
            [ viewButton Refresh ]
        ]
    ]


viewTitle : String -> H.Html msg
viewTitle title =
  H.h1 [] [ H.text title ]


type alias Setting =
  { title : String
  , minutes : Minutes
  }


viewSetting : Setting -> H.Html msg
viewSetting { title, minutes } =
  H.div [ HA.class "setting" ]
    [ H.h2 [ HA.class "setting__title" ] [ H.text title ]
    , H.div [ HA.class "setting__controls" ]
        [ H.div [ HA.class "setting__button" ]
            [ viewButton ArrowDown ]
        , H.span [ HA.class "setting__value" ]
            [ H.text <| Minutes.toString minutes ]
        , H.div [ HA.class "setting__button" ]
            [ viewButton ArrowUp ]
        ]
    ]


type Button
  = ArrowDown
  | ArrowUp
  | PlayPause
  | Refresh


viewButton : Button -> H.Html msg
viewButton button =
  H.button [ HA.class "button" ] <|
    case button of
      ArrowDown ->
        [ H.i [ HA.class "fa fa-arrow-down fa-2x" ] [] ]

      ArrowUp ->
        [ H.i [ HA.class "fa fa-arrow-up fa-2x" ] [] ]

      PlayPause ->
        [ H.i [ HA.class "fa fa-play fa-2x" ] []
        , H.i [ HA.class "fa fa-pause fa-2x" ] []
        ]

      Refresh ->
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
