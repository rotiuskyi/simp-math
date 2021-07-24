module Common.Route exposing (..)

import Browser.Navigation
import Url exposing (Url)


type alias RouteModel =
    { navKey : Browser.Navigation.Key
    }


type Route
    = Root
    | NotFound


init : Browser.Navigation.Key -> RouteModel
init key =
    { navKey = key }


fromUrl : Url -> Route
fromUrl { path } =
    if path == "/" then
        Root

    else
        NotFound
