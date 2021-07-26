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

        expResultOf =
            result operation

        toExp pair =
            let
                variants =
                    List.range (expResultOf pair - 2) (expResultOf pair + 2)
                        |> (::) (expResultOf pair)
                        |> List.filter ((<) 0)
                        |> toUniqueItems
                        |> shacke seed
            in
            Expression operation pair variants Nothing
    in
    Random.map toExp pairGen


result : Operation -> ( Int, Int ) -> Int
result operation pair =
    case operation of
        Addition ->
            Tuple.first pair + Tuple.second pair

        Subtraction ->
            Tuple.first pair - Tuple.second pair

        Multiplication ->
            Tuple.first pair * Tuple.second pair

        Division ->
            Tuple.first pair // Tuple.second pair


answered : Maybe Expression -> Bool
answered mbExp =
    case mbExp of
        Nothing ->
            False

        Just exp ->
            case exp.answer of
                Nothing ->
                    False

                Just _ ->
                    True


answeredAndCorrectly : Maybe Expression -> Bool
answeredAndCorrectly mbExp =
    case mbExp of
        Nothing ->
            False

        Just exp ->
            case ( exp.operation, exp.answer ) of
                ( _, Just answer ) ->
                    result exp.operation exp.arguments == answer

                _ ->
                    False


displayValue : Maybe Expression -> String
displayValue mbExp =
    case mbExp of
        Just exp ->
            let
                answerStr =
                    case exp.answer of
                        Just answer ->
                            String.fromInt answer

                        Nothing ->
                            ""
            in
            String.fromInt (Tuple.first exp.arguments)
                ++ Feature.ExpressionOperation.toString exp.operation
                ++ String.fromInt (Tuple.second exp.arguments)
                ++ resultSign (Just exp)
                ++ answerStr

        Nothing ->
            ""


resultSign : Maybe Expression -> String
resultSign mbExp =
    if answered mbExp then
        if answeredAndCorrectly mbExp then
            " = "

        else
            " ≠ "

    else
        ""
