module Page.Root exposing (RootPgModel, RootPgMsg, init, subscriptions, update, view)

import Bootstrap.Button as Btn
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser.Navigation exposing (Key)
import Dict
import Expression.Expression exposing (Expression, displayValue, equalSign, toEqualSign)
import Expression.Operation as Operation
import Html exposing (Html, label, li, text, ul)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onSubmit)
import Process
import Random
import Route
import Task
import Time
import Url exposing (Url)
import Util.List



-- model


type alias RootPgModel =
    { route : Route.RouteModel
    , currentTime : Time.Posix
    , currentTimeSeed : Random.Seed
    , operation : String
    , expressions : List Expression
    , currExpression : Maybe Expression
    , formDisabled : Bool
    }


init : Url -> Key -> RootPgModel
init _ key =
    { route = Route.init key
    , currentTime = Time.millisToPosix 0
    , currentTimeSeed = Random.initialSeed 0
    , operation = Operation.addition
    , expressions = []
    , currExpression = Nothing
    , formDisabled = False
    }



-- update


type RootPgMsg
    = GotTime Time.Posix
    | SelectOperation String
    | TypedText String
    | GenerateExpressions
    | NewExpressions (List Expression)
    | Answer Int
    | NextExpression


update : RootPgMsg -> RootPgModel -> ( RootPgModel, Cmd RootPgMsg )
update msg model =
    case msg of
        GotTime time ->
            ( { model
                | currentTime = time
                , currentTimeSeed = Random.initialSeed (Time.posixToMillis time)
              }
            , Cmd.none
            )

        SelectOperation operation ->
            ( { model | operation = operation }, Cmd.none )

        TypedText _ ->
            ( model, Cmd.none )

        GenerateExpressions ->
            ( model, generateExpressions model )

        NewExpressions exps ->
            ( { model | expressions = exps, currExpression = List.head exps }, Cmd.none )

        Answer answer ->
            let
                mbCurrExp =
                    model.currExpression

                mbNewCurrExp =
                    case mbCurrExp of
                        Just currExp ->
                            Just { currExp | answer = Just answer }

                        Nothing ->
                            mbCurrExp

                ( currExpression, expressions ) =
                    updateCurrExp mbCurrExp mbNewCurrExp model.expressions
            in
            ( { model
                | currExpression = currExpression
                , expressions = Debug.log "expressions" expressions
                , formDisabled = True
              }
            , Process.sleep 1000
                |> Task.andThen (always <| Task.succeed NextExpression)
                |> Task.perform identity
            )

        NextExpression ->
            let
                mbCurrExp =
                    nextExpression model.currExpression model.expressions

                mbNewCurrExp =
                    case mbCurrExp of
                        Just currExp ->
                            Just { currExp | variants = Util.List.shacke model.currentTimeSeed currExp.variants }

                        Nothing ->
                            mbCurrExp

                ( currExpression, expressions ) =
                    updateCurrExp mbCurrExp mbNewCurrExp model.expressions
            in
            ( { model
                | currExpression = currExpression
                , expressions = expressions
                , formDisabled = False
              }
            , Cmd.none
            )


updateCurrExp : Maybe Expression -> Maybe Expression -> List Expression -> ( Maybe Expression, List Expression )
updateCurrExp mbOld mbNew exps =
    case ( mbOld, mbNew ) of
        ( Just old, Just new ) ->
            ( mbNew
            , List.map
                (\exp ->
                    if exp == old then
                        new

                    else
                        exp
                )
                exps
            )

        ( _, _ ) ->
            ( mbOld, exps )


nextExpression : Maybe Expression -> List Expression -> Maybe Expression
nextExpression mbCurrExp expressions =
    case ( mbCurrExp, expressions ) of
        ( Just currExp, exp :: exps ) ->
            if currExp == exp then
                List.head exps

            else
                nextExpression mbCurrExp exps

        ( _, _ ) ->
            Nothing


generateExpressions : RootPgModel -> Cmd RootPgMsg
generateExpressions model =
    Expression.Expression.generate model.currentTimeSeed Operation.Addition
        |> Random.list 10
        |> Random.andThen (\exps -> filterUniqueExps exps |> Random.constant)
        |> Random.generate NewExpressions


filterUniqueExps : List Expression -> List Expression
filterUniqueExps exps =
    List.foldl (\exp dict -> Dict.insert exp.arguments exp dict) Dict.empty exps |> Dict.values



-- subscriptions


subscriptions : a -> Sub RootPgMsg
subscriptions _ =
    Time.every 1000 GotTime



-- view


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
                            [ Input.text <|
                                withSuccessOption model.currExpression
                                    [ Input.large
                                    , Input.attrs [ class "smc-input smc-input--text-center" ]
                                    , Input.onInput TypedText
                                    , Input.value <| displayValue model.currExpression
                                    ]
                            , variantList model.currExpression model
                            ]
                        ]
                    ]
                ]
            ]
        ]


withSuccessOption : Maybe Expression -> List (Input.Option msg) -> List (Input.Option msg)
withSuccessOption mbExp opts =
    case mbExp of
        Nothing ->
            opts

        Just exp ->
            if toEqualSign (Just exp) == equalSign then
                List.append opts [ Input.success ]

            else
                opts


variantList : Maybe Expression -> RootPgModel -> Html RootPgMsg
variantList maybeExp model =
    case maybeExp of
        Nothing ->
            ul [] []

        Just exp ->
            exp.variants
                |> List.map
                    (\var ->
                        li [ class "smc-exp-variants__item" ]
                            [ Btn.button
                                [ Btn.info
                                , Btn.large
                                , Btn.onClick (Answer var)
                                , Btn.disabled model.formDisabled
                                , Btn.attrs [ type_ "button" ]
                                ]
                                [ text <| String.fromInt var ]
                            ]
                    )
                |> ul [ class "smc-exp-variants" ]
