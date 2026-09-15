module Page.Home exposing (HomePgModel, HomePgMsg, init, subscriptions, update, view)

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



-- model


type alias HomePgModel =
    { route : Route.RouteModel
    , timeMark : Int
    , level : Level
    , operation : Operation
    , expressions : List Expression
    , currExpression : Maybe Expression
    , answeringIsDisabled : Bool
    , answered : Bool
    }


init : Url -> Key -> ( HomePgModel, Cmd HomePgMsg )
init _ key =
    ( { route = Route.init key
      , timeMark = 0
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


type HomePgMsg
    = GotTime Int
    | SelectedLevel String
    | SelectedOperation String
    | GenerateExpressions
    | NewExpressions (List Expression)
    | TypedText String
    | Answer Int
    | AnswerWithTime Int Int
    | NextExpression


update : HomePgMsg -> HomePgModel -> ( HomePgModel, Cmd HomePgMsg )
update msg model =
    case msg of
        GotTime time ->
            ( { model | timeMark = time }, Cmd.none )

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
                mbNextExp =
                    nextExpression model.currExpression model.expressions
            in
            ( { model
                | currExpression = mbNextExp
                , answeringIsDisabled = False
                , answered = mbNextExp == Nothing
              }
            , getTime
            )


getTime : Cmd HomePgMsg
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


generateExpressions : HomePgModel -> Cmd HomePgMsg
generateExpressions model =
    generate model.level model.operation
        |> Random.list 10
        |> Random.map uniqueByArguments
        |> Random.generate NewExpressions



-- subscriptions


subscriptions : a -> Sub HomePgMsg
subscriptions _ =
    Sub.none



-- view


view : HomePgModel -> Html HomePgMsg
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
    HomePgModel
    -> List (Form.Col HomePgMsg)
    -> (List (Form.Col HomePgMsg) -> Html.Html msg)
    -> Html.Html msg
renderWhenAnswering model colOps toMsg =
    case model.currExpression of
        Nothing ->
            text ""

        Just _ ->
            toMsg colOps


answering : HomePgModel -> Bool
answering model =
    case model.currExpression of
        Nothing ->
            False

        Just _ ->
            True


expressionInput : HomePgModel -> Html.Html HomePgMsg
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


variantList : HomePgModel -> Html HomePgMsg
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


resultTable : HomePgModel -> Html.Html msg
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
                            , Table.td [] [ secondsToTenths exp.spentTime |> text ]
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
                                |> secondsToTenths
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


{-| Milliseconds to seconds rounded to tenths, e.g. 1234 -> "1.2".
Integer math avoids float artifacts like "1.2000000000000002".
-}
secondsToTenths : Int -> String
secondsToTenths ms =
    let
        tenths =
            round (toFloat ms / 100)
    in
    String.fromInt (tenths // 10) ++ "." ++ String.fromInt (modBy 10 tenths)


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
