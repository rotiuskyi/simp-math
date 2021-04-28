module Page.Root exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random


type alias RootPgModel =
    Int


init : RootPgModel
init =
    0


type RootPgMsg
    = GenerateRandNumb
    | GeneratedRandNumb Int


update : RootPgMsg -> RootPgModel -> ( RootPgModel, Cmd RootPgMsg )
update msg model =
    case msg of
        GenerateRandNumb ->
            ( model, Random.generate GeneratedRandNumb (Random.int 0 10) )

        GeneratedRandNumb numb ->
            ( numb, Cmd.none )


view : RootPgModel -> Html RootPgMsg
view model =
    div []
        [ button [ onClick GenerateRandNumb ] [ text "Generate Number" ]
        , text <| String.fromInt model
        ]
