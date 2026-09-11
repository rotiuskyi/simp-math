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
import Feature.Expression
    exposing
        ( Expression
        , answeredAndCorrectly
        , correctPercents
        , displayValue
        , generate
        , uniqueByArguments
        )
import Feature.ExpressionLevel as ExpLevel exposing (Level(..))
import Feature.ExpressionOperation as ExpOperation exposing (Operation(..))
import Html exposing (Html, div, h2, label, li, text, ul)
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
    , timeMark : Int
    , timeMarkSeed : Random.Seed
    , level : Level
    , operation : Operation
    , expressions : List Expression
    , currExpression : Maybe Expression
    , answeringIsDisabled : Bool
    , answered : Bool
    }


init : Url -> Key -> ( RootPgModel, Cmd RootPgMsg )
init _ key =
    ( { route = Route.init key
      , timeMark = 0
      , timeMarkSeed = Random.initialSeed 0
      , level = Level1
      , operation = Addition
      , expressions = []
      , currExpression = Nothing
      , answeringIsDisabled = False
      , answered = False
      }
    , getTime
    )



-- update


type RootPgMsg
    = GotTime Int
    | SelectedLevel String
    | SelectedOperation String
    | GenerateExpressions
    | NewExpressions (List Expression)
    | TypedText String
    | Answer Int
    | AnswerWithTime Int Int
    | NextExpression


update : RootPgMsg -> RootPgModel -> ( RootPgModel, Cmd RootPgMsg )
update msg model =
    case msg of
        GotTime time ->
            ( { model
                | timeMark = time
                , timeMarkSeed = Random.initialSeed time
              }
            , Cmd.none
            )

        SelectedLevel level ->
            ( { model | level = ExpLevel.fromString level }, Cmd.none )

        SelectedOperation operation ->
            ( { model | operation = ExpOperation.fromString operation }, Cmd.none )

        GenerateExpressions ->
            ( model, generateExpressions model )

        NewExpressions exps ->
            ( { model
                | expressions = exps
                , currExpression = List.head exps
                , answered = False
              }
            , getTime
            )

        TypedText _ ->
            -- prevent changes
            ( model, Cmd.none )

        Answer answer ->
            ( model
            , Time.now
                |> Task.map Time.posixToMillis
                |> Task.perform (AnswerWithTime answer)
            )

        AnswerWithTime answer time ->
            let
                newCurrExp =
                    case model.currExpression of
                        Just currExp ->
                            Just
                                { currExp
                                    | answer = Just answer
                                    , spentTime = time - model.timeMark
                                }

                        Nothing ->
                            model.currExpression

                ( currExpression, expressions ) =
                    updateCurrExp model.currExpression newCurrExp model.expressions
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
                            Just
                                { currExp
                                    | variants =
                                        Util.List.shacke model.timeMarkSeed currExp.variants
                                }

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
            , getTime
            )


getTime : Cmd RootPgMsg
getTime =
    Time.now
        |> Task.map Time.posixToMillis
        |> Task.perform GotTime


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

        _ ->
            ( mbOld, exps )


nextExpression : Maybe Expression -> List Expression -> Maybe Expression
nextExpression mbCurrExp expressions =
    case ( mbCurrExp, expressions ) of
        ( Just currExp, exp :: exps ) ->
            if currExp == exp then
                List.head exps

            else
                nextExpression mbCurrExp exps

        _ ->
            Nothing


generateExpressions : RootPgModel -> Cmd RootPgMsg
generateExpressions model =
    generate model.timeMarkSeed model.level model.operation
        |> Random.list 10
        |> Random.map uniqueByArguments
        |> Random.generate NewExpressions



-- subscriptions


subscriptions : a -> Sub RootPgMsg
subscriptions _ =
    Sub.none



-- view


view : RootPgModel -> Html RootPgMsg
view model =
    Grid.container [ class "pt-3" ]
        [ div []
            [ Form.form [ onSubmit GenerateExpressions ]
                [ Form.row [ Row.attrs [ class "form-row" ] ]
                    [ Form.col [ Col.xsAuto ]
                        [ label []
                            [ Select.select
                                [ Select.onChange SelectedLevel
                                , Select.attrs [ disabled <| answering model ]
                                ]
                                [ Select.item [ Level1 |> ExpLevel.toString |> value ] [ text "Level 1" ]
                                , Select.item [ Level2 |> ExpLevel.toString |> value ] [ text "Level 2" ]
                                ]
                            ]
                        ]
                    , Form.col [ Col.xsAuto ]
                        [ label []
                            [ Select.select
                                [ Select.onChange SelectedOperation
                                , Select.attrs [ disabled <| answering model ]
                                ]
                                [ Select.item [ Addition |> ExpOperation.toString |> value ] [ text "Addition" ]
                                , Select.item [ Subtraction |> ExpOperation.toString |> value, disabled True ] [ text "Subtraction" ]
                                , Select.item [ Multiplication |> ExpOperation.toString |> value ] [ text "Multiplication" ]
                                , Select.item [ Division |> ExpOperation.toString |> value, disabled True ] [ text "Division" ]
                                ]
                            ]
                        ]
                    ]
                , Form.row []
                    [ Form.col []
                        [ Btn.button
                            [ Btn.primary
                            , Btn.block
                            , Btn.large
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
        , div []
            [ resultTable model ]
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
            if answeredAndCorrectly (Just exp) then
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
                                , Btn.onClick <| Answer var
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
                    , Table.th [] [ text "Spent Time (sec)" ]
                    ]
                ]

        expRows =
            List.indexedMap
                (\idx exp ->
                    Table.tr
                        |> withSuccessOption exp
                            []
                            [ Table.td [] [ idx |> (+) 1 |> String.fromInt |> text ]
                            , Table.td [] [ Just exp |> displayValue |> text ]
                            , Table.td [] [ toFloat exp.spentTime / 1000 |> String.fromFloat |> text ]
                            ]
                )
                model.expressions

        tbody =
            Table.tbody [] <|
                (++) expRows <|
                    [ Table.tr [ Table.rowInfo ]
                        [ Table.td []
                            [ text "Total" ]
                        , Table.td []
                            [ text <| String.fromInt <| correctPercents model.expressions
                            , text "% of correct answers"
                            ]
                        , Table.td []
                            [ List.map (\exp -> exp.spentTime) model.expressions
                                |> List.sum
                                |> toFloat
                                |> (*) 0.001
                                |> String.fromFloat
                                |> text
                            ]
                        ]
                    ]
    in
    if model.answered then
        div []
            [ h2 [] [ text "Result" ]
            , Table.table
                { options = options
                , thead = thead
                , tbody = tbody
                }
            ]

    else
        text ""


withSuccessOption :
    Expression
    -> List (Table.RowOption msg)
    -> List (Table.Cell msg)
    -> (List (Table.RowOption msg) -> List (Table.Cell msg) -> Table.Row msg)
    -> Table.Row msg
withSuccessOption exp rowOps cells toMsg =
    if Just exp |> answeredAndCorrectly then
        toMsg (Table.rowSuccess :: rowOps) cells

    else
        toMsg rowOps cells
