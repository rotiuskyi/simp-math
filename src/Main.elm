module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (Html, a, div, h5, li, text, ul)
import Html.Attributes exposing (class, href)
import Location
import Login
import NotFound
import Root
import RoutePath exposing (RoutePath(..))
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
        case RoutePath.fromString model.url.path of
            RoutePath.Root ->
                [ header, Root.view |> container ]

            RoutePath.Login ->
                [ Login.view model |> container ]

            _ ->
                [ NotFound.view |> container ]
    }


header : Html msg
header =
    div [ class "smc-header" ]
        [ div [ class "container smc-header__content" ]
            [ h5 [ class "smc-header__title" ] [ text "My App" ]
            , ul [ class "smc-header-nav" ]
                [ li [ class "smc-header-nav__item" ] [ a [ RoutePath.toString RoutePath.Login |> href ] [ text "Login" ] ]
                , li [ class "smc-header-nav__item" ] [ a [ RoutePath.toString RoutePath.Root |> href ] [ text "Root" ] ]
                , li [ class "smc-header-nav__item smc-header-nav__item--no-margin" ] [ a [ href "/foo" ] [ text "foo" ] ]
                ]
            ]
        ]


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | GotLoginMsg Login.Msg


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

        GotLoginMsg loginMsg ->
            ( model, Cmd.none )


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
