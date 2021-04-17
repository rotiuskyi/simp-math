module Login exposing (Msg, view)

import Html exposing (Html, div, text)
import Location


type alias Model =
    Location.Model


view : Model -> Html msg
view model =
    div [] [ text "Login" ]


type Msg
    = EnteredRoute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredRoute ->
            ( model, Cmd.none )
