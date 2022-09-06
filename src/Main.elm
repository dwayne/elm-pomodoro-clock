module Main exposing (main)


import Duration exposing (Duration)
import Html as H
import Html.Attributes as HA
import Minutes exposing (Minutes)


main : H.Html msg
main =
  view


view : H.Html msg
view =
  H.div []
    [ H.h2 [] [ H.text "Title" ]
    , viewTitle "25 + 5 Clock"
    , H.h2 [] [ H.text "Setting: Break Length" ]
    , viewSetting "Break Length" <| Minutes.fromInt 5
    , H.h2 [] [ H.text "Setting: Session Length" ]
    , viewSetting "Session Length" <| Minutes.fromInt 25
    , H.h2 [] [ H.text "Display: Break" ]
    , viewDisplay "Break" <| Duration.fromSeconds 300
    , H.h2 [] [ H.text "Display: Session" ]
    , viewDisplay "Session" <| Duration.fromSeconds 1500
    , H.h2 [] [ H.text "Display: Session (Warning)" ]
    , viewDisplay "Session" <| Duration.fromSeconds 59
    ]


viewTitle : String -> H.Html msg
viewTitle title =
  H.h1 [] [ H.text title ]


viewSetting : String -> Minutes -> H.Html msg
viewSetting title minutes =
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


viewDisplay : String -> Duration -> H.Html msg
viewDisplay title duration =
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
