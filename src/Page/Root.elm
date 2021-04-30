module Page.Root exposing (RootPgModel, RootPgMsg, init, update, view)

import Bootstrap.Button as Btn
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Const.Operation as Operation
import Html exposing (Html, label, li, text, ul)
import Html.Attributes exposing (class, disabled, value)
import Html.Events exposing (onSubmit)
import Random
import Set


type alias RootPgModel =
    { randomPairs : List ( Int, Int )
    , pairOperation : String
    , currPair : Maybe ( Int, Int )
    }


init : RootPgModel
init =
    { randomPairs = []
    , pairOperation = Operation.addition
    , currPair = Nothing
    }


type RootPgMsg
    = GeneratePairs
    | GeneratedPairs (List ( Int, Int ))
    | SelectOperation String
    | TypedText String


generatePairs : Cmd RootPgMsg
generatePairs =
    let
        randNum =
            Random.int 1 5
    in
    Random.generate GeneratedPairs <| Random.list 10 <| Random.pair randNum randNum


filterUniquePairs : List ( Int, Int ) -> List ( Int, Int )
filterUniquePairs pairs =
    List.foldl (\pair set -> Set.insert pair set) Set.empty pairs |> Set.toList


update : RootPgMsg -> RootPgModel -> ( RootPgModel, Cmd RootPgMsg )
update msg model =
    case msg of
        GeneratePairs ->
            ( model, generatePairs )

        GeneratedPairs pairs ->
            let
                uniquePairs =
                    filterUniquePairs pairs

                newCurrPair =
                    List.head uniquePairs
            in
            ( { model | randomPairs = uniquePairs, currPair = newCurrPair }, Cmd.none )

        SelectOperation operation ->
            ( { model | pairOperation = operation }, Cmd.none )

        TypedText _ ->
            ( model, Cmd.none )


toInputValue : RootPgModel -> String
toInputValue model =
    let
        operationSymbol =
            if model.pairOperation == Operation.addition then
                " + "

            else if model.pairOperation == Operation.subtraction then
                " - "

            else if model.pairOperation == Operation.multiplication then
                " * "

            else
                " / "
    in
    case model.currPair of
        Just ( i0, i1 ) ->
            String.fromInt i0 ++ operationSymbol ++ String.fromInt i1 ++ " ="

        Nothing ->
            ""


view : RootPgModel -> Html RootPgMsg
view model =
    Grid.container []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ Form.form [ onSubmit GeneratePairs ]
                    [ Form.row []
                        [ Form.col [ Col.xsAuto ]
                            [ label []
                                [ Select.select [ Select.onChange SelectOperation ]
                                    [ Select.item [ value Operation.addition ] [ text Operation.addition ]
                                    , Select.item [ value Operation.multiplication ] [ text Operation.multiplication ]
                                    , Select.item [ value Operation.subtraction, disabled True ] [ text Operation.subtraction ]
                                    , Select.item [ value Operation.division, disabled True ] [ text Operation.division ]
                                    ]
                                ]
                            ]
                        , Form.col []
                            [ Btn.button [ Btn.primary ] [ text "Generate Pairs" ]
                            ]
                        ]
                    , Form.row []
                        [ Form.col []
                            [ Input.text
                                [ Input.large
                                , Input.attrs [ class "smc-input smc-input--text-center" ]
                                , Input.onInput TypedText
                                , Input.value <| toInputValue model
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ text model.pairOperation
                , model.randomPairs
                    |> List.map (\( n0, n1 ) -> li [] [ text <| "( " ++ String.fromInt n0 ++ ", " ++ String.fromInt n1 ++ " )" ])
                    |> ul []
                ]
            ]
        ]
