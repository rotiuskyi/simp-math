module Expression.Expression exposing (..)

import Expression.Operation exposing (Operation)
import Random exposing (Generator)
import Tuple exposing (pair)
import Util.List exposing (shacke, toUniqueItems)


type alias Expression =
    { operation : Operation
    , arguments : ( Int, Int )
    , variants : List Int
    }


generate : Int -> Operation -> Generator Expression
generate forInitialSeed operation =
    let
        intGen =
            Random.int 1 5

        pairGen =
            Random.pair intGen intGen

        toExp pair =
            let
                result =
                    Tuple.first pair + Tuple.second pair

                initialSeed =
                    Random.initialSeed forInitialSeed

                variants =
                    List.range (result - 2) (result + 2)
                        |> (::) result
                        |> List.filter ((<) 0)
                        |> toUniqueItems
                        |> shacke initialSeed
            in
            Expression operation pair variants
    in
    Random.map toExp pairGen


displayValue : Maybe Expression -> String
displayValue maybeExp =
    case maybeExp of
        Just exp ->
            String.fromInt (Tuple.first exp.arguments)
                ++ Expression.Operation.toString exp.operation
                ++ String.fromInt (Tuple.second exp.arguments)
                ++ " = "

        Nothing ->
            ""
