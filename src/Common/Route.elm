module Common.Route exposing (..)

import Browser.Navigation
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


type alias RouteModel =
    { navKey : Browser.Navigation.Key
    }


init : Browser.Navigation.Key -> RouteModel
init key =
    { navKey = key }


type Route
    = Home


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Home (s "index.html")
        ]


{-| GitHub Pages serves the app under /simp-math/, locally it is served from the root.
-}
fromUrl : Url -> Maybe Route
fromUrl url =
    parse (oneOf [ parser, s "simp-math" </> parser ]) url
