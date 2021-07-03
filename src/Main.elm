module Main exposing (main)

import AppPath as AppPath
import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html.Attributes exposing (href)
import Page.NotFound
import Page.Root
import Shell
import Url exposing (Url)


type alias Model =
    { currUrl : Url
    , navKey : Key
    , rootModel : Page.Root.RootPgModel
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { currUrl = url
      , navKey = key
      , rootModel = Page.Root.init
      }
    , Cmd.none
    )


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | GotRootPgMsg Page.Root.RootPgMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            ( { model | currUrl = url }, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, load href )

        GotRootPgMsg subMsg ->
            let
                ( newRootModel, cmd ) =
                    Page.Root.update subMsg model.rootModel
            in
            ( { model | rootModel = newRootModel }, Cmd.map GotRootPgMsg cmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    let
        container pathname =
            if pathname == AppPath.root then
                [ Shell.view GotRootPgMsg <| Page.Root.view model.rootModel ]

            else
                [ Page.NotFound.view ]
    in
    { title = "Simple Math", body = container model.currUrl.path }


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
