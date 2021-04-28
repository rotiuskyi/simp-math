module Shell exposing (view)

import AppPath exposing (appPath)
import Html exposing (Html, a, div, h5, li, text, ul)
import Html.Attributes exposing (class, href)


header : Html msg
header =
    div [ class "smc-header" ]
        [ div [ class "container smc-header__content" ]
            [ h5 [ class "smc-header__title" ] [ text "My App" ]
            , ul [ class "smc-header-nav" ]
                [ li [ class "smc-header-nav__item" ] [ a [ href appPath.root ] [ text "Root" ] ]
                , li [ class "smc-header-nav__item smc-header-nav__item--no-margin" ] [ a [ href "/foo" ] [ text "foo" ] ]
                ]
            ]
        ]


view : (a -> msg) -> Html a -> Html msg
view toMsg content =
    div []
        [ header
        , Html.map toMsg content
        ]
