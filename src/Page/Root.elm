module Page.Root exposing (RootPgModel, RootPgMsg, init, update, view)

import Bootstrap.Button as Btn
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser.Navigation exposing (Key)
import Dict
import Expression.Expression exposing (Expression, displayValue)
import Expression.Operation as Operation
import Html exposing (Html, label, li, text, ul)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onSubmit)
import Random
import Route
import Url exposing (Url)


type alias RootPgModel =
    { route : Route.RouteModel
    , operation : String
    , expressions : List Expression
    , currExpression : Maybe Expression
    }


type RootPgMsg
    = GenerateExpressions
    | NewExpressions (List Expression)
    | SelectOperation String
    | TypedText String


init : Url -> Key -> RootPgModel
init _ key =
    { route = Route.init key
    , operation = Operation.addition
    , expressions = []
    , currExpression = Nothing
    }


update : RootPgMsg -> RootPgModel -> ( RootPgModel, Cmd RootPgMsg )
update msg model =
    case msg of
        GenerateExpressions ->
            ( model, generateExpressions )

        NewExpressions exps ->
            ( { model | expressions = exps, currExpression = List.head exps }, Cmd.none )

        SelectOperation operation ->
            ( { model | operation = operation }, Cmd.none )

        TypedText _ ->
            ( model, Cmd.none )


generateExpressions : Cmd RootPgMsg
generateExpressions =
    Expression.Expression.generate Operation.Addition
        |> Random.list 10
        |> Random.andThen (\exps -> filterUniqueExps exps |> Random.constant)
        |> Random.generate NewExpressions


filterUniqueExps : List Expression -> List Expression
filterUniqueExps exps =
    List.foldl (\exp dict -> Dict.insert exp.arguments exp dict) Dict.empty exps |> Dict.values


view : RootPgModel -> Html RootPgMsg
view model =
    Grid.container []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ Form.form [ onSubmit GenerateExpressions ]
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
                                , Input.value <| displayValue model.currExpression
                                ]
                            , variantList model.currExpression
                            ]
                        ]
                    ]
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ argumentList model.expressions
                ]
            ]
        ]


variantList : Maybe Expression -> Html msg
variantList maybeExp =
    case maybeExp of
        Nothing ->
            ul [] []

        Just exp ->
            exp.variants
                |> List.map (\var -> li [ class "smc-exp-variants__item" ] [ Btn.button [ Btn.info, Btn.large, Btn.attrs [ type_ "button" ] ] [ text <| String.fromInt var ] ])
                |> ul [ class "smc-exp-variants" ]


argumentList : List Expression -> Html msg
argumentList expressions =
    expressions
        |> List.map (\exp -> li [] [ text <| "( " ++ String.fromInt (Tuple.first exp.arguments) ++ ", " ++ String.fromInt (Tuple.second exp.arguments) ++ " )" ])
        |> ul []
