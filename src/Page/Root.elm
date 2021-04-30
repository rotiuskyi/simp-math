module Page.Root exposing (RootPgModel, RootPgMsg, init, update, view)

import Bootstrap.Button as Btn
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Const.Operation as Operation
import Html exposing (Html, label, li, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onSubmit)
import Random


type alias RootPgModel =
    { randomPairs : List ( Int, Int )
    , operation : String
    }


init : RootPgModel
init =
    { randomPairs = []
    , operation = Operation.addition
    }


type RootPgMsg
    = GenerateRandomPairs
    | GeneratedRandomPairs (List ( Int, Int ))
    | SelectOperation String


generatePairs : Cmd RootPgMsg
generatePairs =
    let
        randNum =
            Random.int 1 9
    in
    Random.generate GeneratedRandomPairs <| Random.list 10 <| Random.pair randNum randNum


update : RootPgMsg -> RootPgModel -> ( RootPgModel, Cmd RootPgMsg )
update msg model =
    case msg of
        GenerateRandomPairs ->
            ( model, generatePairs )

        GeneratedRandomPairs pairs ->
            ( { model | randomPairs = pairs }, Cmd.none )

        SelectOperation operation ->
            ( { model | operation = operation }, Cmd.none )


view : RootPgModel -> Html RootPgMsg
view model =
    Grid.container []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ Form.form [ onSubmit GenerateRandomPairs ]
                    [ Form.row []
                        [ Form.col [ Col.xsAuto ]
                            [ label []
                                [ Select.select [ Select.onChange SelectOperation ]
                                    [ Select.item [ value Operation.addition ] [ text Operation.addition ]
                                    , Select.item [ value Operation.subtraction ] [ text Operation.subtraction ]
                                    , Select.item [ value Operation.multiplication ] [ text Operation.multiplication ]
                                    , Select.item [ value Operation.subtraction ] [ text Operation.subtraction ]
                                    ]
                                ]
                            ]
                        , Form.col []
                            [ Btn.button [ Btn.primary ] [ text "Generate Pairs" ]
                            ]
                        ]
                    ]
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ text model.operation
                , model.randomPairs
                    |> List.map (\( n0, n1 ) -> li [] [ text <| String.fromInt n0 ++ "-" ++ String.fromInt n1 ])
                    |> ul []
                ]
            ]
        ]
