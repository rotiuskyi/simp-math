module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html.Attributes exposing (href)
import Page.NotFound
import Page.Root
import Route
import Shell
import Url exposing (Url)


type Model
    = NotFound Page.NotFound.NotFoundPgModel
    | Root Page.Root.RootPgModel


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | GotRootPgMsg Page.Root.RootPgMsg


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( let
        route =
            Route.fromUrl url
      in
      case route of
        Route.NotFound ->
            Page.NotFound.init key |> NotFound

        Route.Root ->
            Page.Root.init url key |> Root
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        routeModel =
                            toRouteModel model

                        urlStr =
                            Url.toString url
                    in
                    ( model, pushUrl routeModel.navKey urlStr )

                Browser.External href ->
                    ( model, load href )

        ( GotRootPgMsg rootMsg, Root rootModel ) ->
            Page.Root.update rootMsg rootModel |> updateWith GotRootPgMsg Root

        ( _, _ ) ->
            ( model, Cmd.none )


toRouteModel : Model -> Route.RouteModel
toRouteModel model =
    case model of
        Root rootModel ->
            rootModel.route

        NotFound nfModel ->
            nfModel


updateWith : (subMsg -> Msg) -> (subModel -> Model) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toMsg toModel ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    let
        title =
            "Simple Math"
    in
    case model of
        Root rootModel ->
            { title = title, body = [ Page.Root.view rootModel |> Shell.view GotRootPgMsg ] }

        NotFound nfModel ->
            { title = title, body = [ Page.NotFound.view ] }


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
