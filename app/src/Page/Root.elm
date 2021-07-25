module Page.Root exposing (RootPgModel, RootPgMsg, init, subscriptions, update, view)

import Bootstrap.Button as Btn
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table exposing (TBody(..))
import Browser.Navigation exposing (Key)
import Common.Route as Route
import Dict
import Feature.Expression exposing (Expression, displayValue, equalSign, toEqualSign)
import Feature.ExpressionOperation as ExpOperation exposing (Operation(..))
import Html exposing (Html, div, h1, label, li, text, ul)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onSubmit)
import Process
import Random
import Task
import Time
import Url exposing (Url)
import Util.List



-- model


type alias RootPgModel =
    { route : Route.RouteModel
    , currentTime : Time.Posix
    , currentTimeSeed : Random.Seed
    , operation : Operation
    , expressions : List Expression
    , currExpression : Maybe Expression
    , answeringIsDisabled : Bool
    , answered : Bool
    }


init : Url -> Key -> RootPgModel
init _ key =
    { route = Route.init key
    , currentTime = Time.millisToPosix 0
    , currentTimeSeed = Random.initialSeed 0
    , operation = Addition
    , expressions = []
    , currExpression = Nothing
    , answeringIsDisabled = False
    , answered = False
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
            ( { model | operation = ExpOperation.fromString operation }, Cmd.none )

        TypedText _ ->
            ( model, Cmd.none )

        GenerateExpressions ->
            ( model, generateExpressions model )

        NewExpressions exps ->
            ( { model
                | expressions = exps
                , currExpression = List.head exps
                , answered = False
              }
            , Cmd.none
            )

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
                , expressions = expressions
                , answeringIsDisabled = True
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

                answered =
                    case mbNewCurrExp of
                        Nothing ->
                            True

                        Just _ ->
                            False

                ( currExpression, expressions ) =
                    updateCurrExp mbCurrExp mbNewCurrExp model.expressions
            in
            ( { model
                | currExpression = currExpression
                , expressions = expressions
                , answeringIsDisabled = False
                , answered = answered
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
    Feature.Expression.generate model.currentTimeSeed model.operation
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
        [ div [ class "page-header" ]
            [ h1 []
                [ text "Unit #1" ]
            ]
        , Grid.row []
            [ Grid.col []
                [ Form.form [ onSubmit GenerateExpressions ]
                    [ Form.row []
                        [ Form.col [ Col.xsAuto ]
                            [ label []
                                [ Select.select
                                    [ Select.onChange SelectOperation
                                    , Select.attrs [ disabled <| answering model ]
                                    ]
                                    [ Select.item [ Addition |> ExpOperation.toString |> value ] [ text "Addition" ]
                                    , Select.item [ Subtraction |> ExpOperation.toString |> value, disabled True ] [ text "Subtraction" ]
                                    , Select.item [ Multiplication |> ExpOperation.toString |> value ] [ text "Multiplication" ]
                                    , Select.item [ Division |> ExpOperation.toString |> value, disabled True ] [ text "Division" ]
                                    ]
                                ]
                            ]
                        , Form.col []
                            [ Btn.button
                                [ Btn.primary
                                , Btn.attrs [ disabled <| answering model ]
                                ]
                                [ text "Start Answering" ]
                            ]
                        ]
                    , Form.row
                        []
                        |> renderWhenAnswering model
                            [ Form.col []
                                [ expressionInput model ]
                            ]
                    , Form.row
                        [ Row.centerXs ]
                        |> renderWhenAnswering model
                            [ Form.col [ Col.xsAuto ]
                                [ variantList model ]
                            ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ resultTable model ]
            ]
        ]


renderWhenAnswering :
    RootPgModel
    -> List (Form.Col RootPgMsg)
    -> (List (Form.Col RootPgMsg) -> Html.Html msg)
    -> Html.Html msg
renderWhenAnswering model colOps toMsg =
    case model.currExpression of
        Nothing ->
            text ""

        Just _ ->
            toMsg colOps


answering : RootPgModel -> Bool
answering model =
    case model.currExpression of
        Nothing ->
            False

        Just _ ->
            True


expressionInput : RootPgModel -> Html.Html RootPgMsg
expressionInput model =
    let
        defaultOpts =
            [ Input.large
            , Input.attrs [ class "expression-input" ]
            , Input.value <| displayValue model.currExpression
            , Input.onInput TypedText
            ]

        defaultInput =
            Input.text defaultOpts

        successInput =
            Input.text <| Input.success :: defaultOpts
    in
    case model.currExpression of
        Nothing ->
            text ""

        Just exp ->
            if toEqualSign (Just exp) == equalSign then
                successInput

            else
                defaultInput


variantList : RootPgModel -> Html RootPgMsg
variantList model =
    let
        list =
            ul [ class "variant-list" ]
    in
    case model.currExpression of
        Nothing ->
            text ""

        Just exp ->
            exp.variants
                |> List.map
                    (\var ->
                        li [ class "variant-list__item" ]
                            [ Btn.button
                                [ Btn.info
                                , Btn.large
                                , Btn.attrs [ type_ "button" ]
                                , Btn.disabled model.answeringIsDisabled
                                , Btn.onClick (Answer var)
                                ]
                                [ text <| String.fromInt var ]
                            ]
                    )
                |> list


resultTable : RootPgModel -> Html.Html msg
resultTable model =
    let
        options =
            []

        thead =
            Table.thead []
                [ Table.tr []
                    [ Table.th [] [ text "#" ]
                    , Table.th [] [ text "Expression" ]
                    ]
                ]

        tbody =
            Table.tbody [] <|
                List.indexedMap
                    (\idx exp ->
                        Table.tr []
                            [ Table.td [] [ idx |> (+) 1 |> String.fromInt |> text ]
                            , Table.td [] [ Just exp |> displayValue |> text ]
                            ]
                    )
                    model.expressions
    in
    if model.answered then
        Table.table
            { options = options
            , thead = thead
            , tbody = tbody
            }

    else
        text ""
