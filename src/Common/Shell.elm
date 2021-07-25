module Common.Shell exposing (view)

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)


header : Html msg
header =
    div [ class "header" ]
        [ div [ class "container logo-box" ]
            [ img [ class "logo-box__logo", src "/favicon.ico" ] []
            ]
        ]


view : (a -> msg) -> Html a -> Html msg
view toMsg content =
    div []
        [ header
        , Html.map toMsg content
        ]
