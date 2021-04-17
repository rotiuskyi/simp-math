module Location exposing (Model)

import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias Model =
    { key : Key
    , url : Url
    }
