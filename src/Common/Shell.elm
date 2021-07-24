module Common.Shell exposing (view)

import Html exposing (Html, div, h5, text)
import Html.Attributes exposing (class)


header : Html msg
header =
    div [ class "smc-header" ]
        [ div [ class "container smc-header__content" ]
            [ h5 [ class "smc-header__title" ] [ text "Simple Math" ]
            ]
        ]


view : (a -> msg) -> Html a -> Html msg
view toMsg content =
    div []
        [ header
        , Html.map toMsg content
        ]
