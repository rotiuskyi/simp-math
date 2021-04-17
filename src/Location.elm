module Location exposing (Model, paths)

import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias Model =
    { key : Key
    , url : Url
    }


paths : { root : String, notFound : String }
paths =
    { root = "/"
    , notFound = "/not_found"
    }
