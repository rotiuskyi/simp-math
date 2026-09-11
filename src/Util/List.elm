module Util.List exposing (..)

import Random exposing (Seed)
import Set


toUniqueItems : List comparable -> List comparable
toUniqueItems list =
    Set.fromList list
        |> Set.toList


shacke : Seed -> List a -> List a
shacke initialSeed items =
    List.foldl
        (\item ( list, seed ) ->
            let
                ( weight, nextSeed ) =
                    Random.step (Random.int 0 Random.maxInt) seed
            in
            ( ( item, weight ) :: list, nextSeed )
        )
        ( [], initialSeed )
        items
        -- get list
        |> Tuple.first
        -- then sort by weight
        |> List.sortBy Tuple.second
        -- then get original values
        |> List.map Tuple.first
