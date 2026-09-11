module Feature.ExpressionLevel exposing (Level(..), argumentRange, fromString, toString)


type Level
    = Level1
    | Level2


toString : Level -> String
toString level =
    case level of
        Level1 ->
            "1"

        Level2 ->
            "2"


fromString : String -> Level
fromString level =
    if level == toString Level2 then
        Level2

    else
        Level1


{-| Inclusive range expression arguments are generated in.
Level 2 covers the multiplication table.
-}
argumentRange : Level -> ( Int, Int )
argumentRange level =
    case level of
        Level1 ->
            ( 1, 5 )

        Level2 ->
            ( 2, 9 )
