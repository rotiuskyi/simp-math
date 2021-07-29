module Common.Route exposing (..)

import Browser.Navigation
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, top)


type alias RouteModel =
    { navKey : Browser.Navigation.Key
    }


init : Browser.Navigation.Key -> RouteModel
init key =
    { navKey = key }


type Route
    = Root


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Root top
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    parse parser url
