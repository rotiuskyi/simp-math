module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Common.Route as Route
import Common.Shell as Shell
import Html.Attributes exposing (href)
import Page.NotFound
import Page.Root
import Url exposing (Url)



-- model


type Model
    = NotFound Page.NotFound.NotFoundPgModel
    | Root Page.Root.RootPgModel


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    case Route.fromUrl url of
        Nothing ->
            Page.NotFound.init key
                |> withMapBy identity NotFound

        Just Route.Root ->
            Page.Root.init url key
                |> withMapBy GotRootPgMsg Root


withMapBy : (a -> Msg) -> (b -> Model) -> ( b, Cmd a ) -> ( Model, Cmd Msg )
withMapBy toMsg toModel ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )



-- update


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | GotRootPgMsg Page.Root.RootPgMsg


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
            Page.Root.update rootMsg rootModel |> withMapBy GotRootPgMsg Root

        ( _, _ ) ->
            ( model, Cmd.none )


toRouteModel : Model -> Route.RouteModel
toRouteModel model =
    case model of
        Root rootModel ->
            rootModel.route

        NotFound nfModel ->
            nfModel



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Page.Root.subscriptions model |> Sub.map GotRootPgMsg



-- view


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



-- main


main : Program () Model Msg
main =
    application
        { onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
