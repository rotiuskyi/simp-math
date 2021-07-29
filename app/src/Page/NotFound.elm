module Page.NotFound exposing (NotFoundPgModel, init, view)

import Browser.Navigation exposing (Key)
import Common.Route as Route
import Html exposing (Html, div, text)


type alias NotFoundPgModel =
    Route.RouteModel


init : Key -> ( NotFoundPgModel, Cmd msg )
init key =
    ( Route.init key, Cmd.none )


view : Html msg
view =
    div [] [ text "Not Found" ]
