module Page.NotFound exposing (NotFoundPgModel, init, view)

import Browser.Navigation exposing (Key)
import Html exposing (Html, div, text)
import Route


type alias NotFoundPgModel =
    Route.RouteModel


init : Key -> NotFoundPgModel
init key =
    Route.init key


view : Html msg
view =
    div [] [ text "Not Found" ]
