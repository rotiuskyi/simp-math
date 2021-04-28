module AppPath exposing (..)


type alias AppPath =
    { root : String
    , notFound : String
    }


appPath : AppPath
appPath =
    AppPath "/" "/not_found"
