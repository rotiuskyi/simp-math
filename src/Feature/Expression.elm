module Feature.Expression exposing (..)

import Feature.ExpressionOperation exposing (Operation(..))
import Random exposing (Generator, Seed)
import Tuple exposing (pair)
import Util.List exposing (shacke, toUniqueItems)


type alias Expression =
    { operation : Operation
    , arguments : ( Int, Int )
    , variants : List Int
    , answer : Maybe Int
    }


generate : Seed -> Operation -> Generator Expression
generate seed operation =
    let
        intGen =
            Random.int 1 5

        pairGen =
            Random.pair intGen intGen

        result : ( Int, Int ) -> Int
        result pair =
            case operation of
                Addition ->
                    Tuple.first pair + Tuple.second pair

                Subtraction ->
                    Tuple.first pair - Tuple.second pair

                Multiplication ->
                    Tuple.first pair * Tuple.second pair

                Division ->
                    Tuple.first pair // Tuple.second pair

        toExp pair =
            let
                variants =
                    List.range (result pair - 2) (result pair + 2)
                        |> (::) (result pair)
                        |> List.filter ((<) 0)
                        |> toUniqueItems
                        |> shacke seed
            in
            Expression operation pair variants Nothing
    in
    Random.map toExp pairGen


displayValue : Maybe Expression -> String
displayValue mbExp =
    let
        answerStr =
            case mbExp of
                Just exp ->
                    case exp.answer of
                        Just answer ->
                            String.fromInt answer

                        Nothing ->
                            ""

                Nothing ->
                    ""
    in
    case mbExp of
        Just exp ->
            String.fromInt (Tuple.first exp.arguments)
                ++ Feature.ExpressionOperation.toString exp.operation
                ++ String.fromInt (Tuple.second exp.arguments)
                ++ toEqualSign (Just exp)
                ++ answerStr

        Nothing ->
            ""


equalSign : String
equalSign =
    " = "


notEqualSign : String
notEqualSign =
    " ≠ "


toEqualSign : Maybe Expression -> String
toEqualSign mbExp =
    case mbExp of
        Nothing ->
            ""

        Just exp ->
            let
                first =
                    Tuple.first exp.arguments

                second =
                    Tuple.second exp.arguments
            in
            case ( exp.operation, exp.answer ) of
                ( Addition, Just answer ) ->
                    if first + second == answer then
                        equalSign

                    else
                        notEqualSign

                ( Subtraction, Just answer ) ->
                    if first - second == answer then
                        equalSign

                    else
                        notEqualSign

                ( Multiplication, Just answer ) ->
                    if first * second == answer then
                        equalSign

                    else
                        notEqualSign

                ( Division, Just answer ) ->
                    if toFloat first / toFloat second == toFloat answer then
                        equalSign

                    else
                        notEqualSign

                _ ->
                    ""
