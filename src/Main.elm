module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (Html, a, div, h5, li, text, ul)
import Html.Attributes exposing (class, href)
import Location
import NotFound
import Root
import Url exposing (Url)


type alias Model =
    Location.Model


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Location.Model key url, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Simple Math"
    , body =
        let
            container content =
                div [ class "container" ] [ content ]
        in
        if model.url.path == Location.paths.root then
            [ header, container Root.view ]

        else
            [ container NotFound.view ]
    }


header : Html msg
header =
    div [ class "smc-header" ]
        [ div [ class "container smc-header__content" ]
            [ h5 [ class "smc-header__title" ] [ text "My App" ]
            , ul [ class "smc-header-nav" ]
                [ li [ class "smc-header-nav__item" ] [ a [ href Location.paths.root ] [ text "Root" ] ]
                , li [ class "smc-header-nav__item smc-header-nav__item--no-margin" ] [ a [ href "/foo" ] [ text "foo" ] ]
                ]
            ]
        ]


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            ( { model | url = url }
            , Cmd.none
            )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, load href )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
