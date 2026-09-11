module Feature.Expression exposing (..)

import Dict exposing (Dict)
import Feature.ExpressionLevel exposing (Level, argumentRange)
import Feature.ExpressionOperation exposing (Operation(..))
import Random exposing (Generator)
import Tuple exposing (pair)
import Util.List exposing (shuffle, unique)


type alias Expression =
    { operation : Operation
    , arguments : ( Int, Int )
    , variants : List Int
    , answer : Maybe Int
    , spentTime : Int
    }


generate : Level -> Operation -> Generator Expression
generate level operation =
    let
        ( minArg, maxArg ) =
            argumentRange level

        intGen =
            Random.int minArg maxArg

        toExp arguments variants =
            Expression operation arguments variants Nothing 0
    in
    Random.pair intGen intGen
        |> Random.andThen
            (\arguments -> Random.map (toExp arguments) (generateVariants operation arguments))


variantCount : Int
variantCount =
    5


{-| Sorted answer variants: the correct answer and wrong ones taken from `mistakeTiers`.
The position of the correct answer among the variants is random,
so it can't be guessed as e.g. the middle one.
-}
generateVariants : Operation -> ( Int, Int ) -> Generator (List Int)
generateVariants operation arguments =
    let
        answer =
            result operation arguments

        pickVariants mistakes =
            let
                below =
                    List.filter ((>) answer) mistakes

                above =
                    List.filter ((<) answer) mistakes

                toVariants belowCount =
                    List.take belowCount below
                        ++ answer
                        :: List.take (variantCount - 1 - belowCount) above
                        |> List.sort
            in
            case
                List.range 0 (variantCount - 1)
                    |> List.filter (\n -> n <= List.length below && variantCount - 1 - n <= List.length above)
            of
                first :: rest ->
                    Random.uniform first rest |> Random.map toVariants

                [] ->
                    Random.constant <| List.sort (answer :: List.take (variantCount - 1) mistakes)
    in
    mistakeTiers operation arguments
        |> List.map (preferSameParity operation answer >> shuffleTier)
        |> List.foldr (Random.map2 (++)) (Random.constant [])
        |> Random.map (unique >> List.filter (\x -> x > 0 && x /= answer))
        |> Random.andThen pickVariants


shuffleTier : ( List Int, List Int ) -> Generator (List Int)
shuffleTier ( preferred, rest ) =
    Random.map2 (++) (shuffle preferred) (shuffle rest)


{-| For multiplication, wrong answers with a different parity are easy to reject
(e.g. 7 · 8 can't be odd), so they go after the ones with the same parity.
-}
preferSameParity : Operation -> Int -> List Int -> ( List Int, List Int )
preferSameParity operation answer tier =
    case operation of
        Multiplication ->
            List.partition (\x -> modBy 2 x == modBy 2 answer) tier

        _ ->
            ( tier, [] )


{-| Wrong answers grouped by how typical the mistake is, most typical first.
-}
mistakeTiers : Operation -> ( Int, Int ) -> List (List Int)
mistakeTiers operation ( a, b ) =
    let
        answer =
            result operation ( a, b )
    in
    case operation of
        Multiplication ->
            [ -- neighbours in the multiplication table
              [ (a - 1) * b, (a + 1) * b, a * (b - 1), a * (b + 1) ]
            , -- the same last digit
              if answer >= 20 then
                [ answer - 10, answer + 10 ]

              else
                []
            , -- swapped digits: 56 -> 65
              swappedDigits answer
            , [ answer - 2, answer + 2, answer - 1, answer + 1 ]
            , -- reserve for small answers like 1 · 1
              [ answer - 4, answer - 3 ] ++ List.range (answer + 3) (answer + 6)
            ]

        _ ->
            [ [ answer - 1, answer + 1 ]
            , [ answer - 2, answer + 2 ]
            , -- forgot to carry the ten: 7 + 8 -> 5
              if answer > 10 then
                [ answer - 10 ]

              else
                []
            , [ answer - 3, answer + 3 ]
            , -- reserve for small answers like 1 + 1
              answer - 4 :: List.range (answer + 4) (answer + 6)
            ]


swappedDigits : Int -> List Int
swappedDigits n =
    if n > 9 && modBy 10 n /= 0 then
        String.fromInt n
            |> String.reverse
            |> String.toInt
            |> Maybe.map List.singleton
            |> Maybe.withDefault []

    else
        []


uniqueByArguments : List Expression -> List Expression
uniqueByArguments exps =
    List.foldl (\exp dict -> Dict.insert exp.arguments exp dict) Dict.empty exps |> Dict.values


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


correctPercents : List Expression -> Int
correctPercents exps =
    let
        totalCount =
            List.length exps

        correctCount =
            List.foldl
                (\exp acc ->
                    if Just exp |> answeredAndCorrectly then
                        acc + 1

                    else
                        acc
                )
                0
                exps
    in
    toFloat correctCount / toFloat totalCount * 100 |> round


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
