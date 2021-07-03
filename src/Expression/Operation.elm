module Expression.Operation exposing (..)


type Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division


addition : String
addition =
    "Addition"


subtraction : String
subtraction =
    "Subtraction"


multiplication : String
multiplication =
    "Multiplication"


division : String
division =
    "Division"


toString : Operation -> String
toString operation =
    case operation of
        Addition ->
            " + "

        Multiplication ->
            " * "

        Subtraction ->
            " - "

        Division ->
            " / "


fromString : String -> Operation
fromString operation =
    if operation == addition then
        Addition

    else if operation == multiplication then
        Multiplication

    else if operation == subtraction then
        Subtraction

    else if operation == division then
        Division

    else
        Addition
