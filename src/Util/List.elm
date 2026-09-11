module Util.List exposing (shuffle, unique)

import Random exposing (Generator)
import Set


{-| Removes duplicates keeping the first occurrence of each item.
-}
unique : List comparable -> List comparable
unique list =
    List.foldl
        (\item ( seen, acc ) ->
            if Set.member item seen then
                ( seen, acc )

            else
                ( Set.insert item seen, item :: acc )
        )
        ( Set.empty, [] )
        list
        |> Tuple.second
        |> List.reverse


shuffle : List a -> Generator (List a)
shuffle items =
    Random.list (List.length items) (Random.float 0 1)
        |> Random.map
            (\weights ->
                List.map2 Tuple.pair weights items
                    -- sort by weight
                    |> List.sortBy Tuple.first
                    -- then get original values
                    |> List.map Tuple.second
            )
