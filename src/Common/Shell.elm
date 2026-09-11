module Common.Shell exposing (view)

import Html exposing (Html, div, img, small, span, text)
import Html.Attributes exposing (class, src)


header : Html msg
header =
    div [ class "shell__header" ]
        [ div [ class "container logo-box" ]
            [ img [ class "logo-box__logo", src "assets/favicon.ico" ] []
            , span [ class "logo-box__title" ] [ text "Simple Math" ]
            ]
        ]


footer : Html msg
footer =
    div [ class "shell__footer" ]
        [ div [ class "container" ]
            [ small [] [ text "Created by Roman Otiuskyi" ] ]
        ]


view : (a -> msg) -> Html a -> Html msg
view toMsg content =
    div [ class "shell" ]
        [ header
        , Html.map toMsg <| div [ class "shell__content" ] [ content ]
        , footer
        ]
