module Page.Root exposing (..)

import Html exposing (Html, button, div, li, text, ul)
import Html.Events exposing (onClick)
import Random


type alias RootPgModel =
    List ( Int, Int )


init : RootPgModel
init =
    []


type RootPgMsg
    = GenerateRandomPairs
    | GeneratedRandomPairs (List ( Int, Int ))


generatePairs : Cmd RootPgMsg
generatePairs =
    let
        randNum =
            Random.int 0 5
    in
    Random.generate GeneratedRandomPairs <| Random.list 10 <| Random.pair randNum randNum


update : RootPgMsg -> RootPgModel -> ( RootPgModel, Cmd RootPgMsg )
update msg model =
    case msg of
        GenerateRandomPairs ->
            ( model, generatePairs )

        GeneratedRandomPairs pairs ->
            ( pairs, Cmd.none )


view : RootPgModel -> Html RootPgMsg
view model =
    div []
        [ button [ onClick GenerateRandomPairs ] [ text "Generate Pairs" ]
        , ul [] <| List.map (\( n0, n1 ) -> li [] [ text <| String.fromInt n0 ++ "-" ++ String.fromInt n1 ]) <| model
        ]
