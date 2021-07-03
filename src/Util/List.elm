module Util.List exposing (toUniqueItems)

import Set


toUniqueItems : List comparable -> List comparable
toUniqueItems list =
    Set.fromList list
        |> Set.toList
