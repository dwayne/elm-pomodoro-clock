module Main exposing (main)


import Browser
import Html exposing (Html, a, button, div, footer, h1, h2, h3, i, span, text)
import Html.Attributes exposing (class, href, target)


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


-- MODEL


type alias Model = ()


init : Model
init = ()


-- UPDATE


type alias Msg = Never


update : Msg -> Model -> Model
update _ = identity


-- VIEW


view : Model -> Html msg
view _ =
  div []
    [ h1 [] [ text "Pomodoro Clock" ]
    , div [ class "flex" ]
        [ div [ class "setting" ]
            [ h2 [] [ text "Break Length" ]
            , div []
                [ button [ class "button" ]
                    [ i [ class "fa fa-arrow-down fa-2x" ] [] ]
                , span [] [ text "5" ]
                , button [ class "button" ]
                    [ i [ class "fa fa-arrow-up fa-2x" ] [] ]
                ]
            ]
        , div [ class "setting" ]
            [ h2 [] [ text "Session Length" ]
            , div []
                [ button [ class "button" ]
                    [ i [ class "fa fa-arrow-down fa-2x" ] [] ]
                , span [] [ text "25" ]
                , button [ class "button" ]
                    [ i [ class "fa fa-arrow-up fa-2x" ] [] ]
                ]
            ]
        ]
    , div [ class "mt timer" ]
        [ h3 [ class "timer__title" ] [ text "Session" ]
        , div [ class "timer__value" ] [ text "25:00" ]
        ]
    , div [ class "mt" ]
        [ button [ class "button" ]
            [ i [ class "fa fa-play fa-2x" ] []
            , i [ class "fa fa-pause fa-2x" ] []
            ]
        , button [ class "button" ]
            [ i [ class "fa fa-refresh fa-2x" ] [] ]
        ]
    , footer [ class "mt attribution" ]
        [ text "Developed by"
        , a
          [ class "developer"
          , href "https://github.com/dwayne/"
          , target "_blank"
          ]
          [ text "Dwayne Crooks" ]
        ]
    ]
