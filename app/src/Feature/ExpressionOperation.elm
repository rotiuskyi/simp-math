module Feature.ExpressionOperation exposing (Operation(..), fromString, toString)


type Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division


additionSign : String
additionSign =
    " + "


substractionSign : String
substractionSign =
    " - "


multiplicationSign : String
multiplicationSign =
    " · "


divisionSign : String
divisionSign =
    " ÷ "


toString : Operation -> String
toString operation =
    case operation of
        Addition ->
            additionSign

        Subtraction ->
            substractionSign

        Multiplication ->
            multiplicationSign

        Division ->
            divisionSign


fromString : String -> Operation
fromString operation =
    if operation == additionSign then
        Addition

    else if operation == substractionSign then
        Subtraction

    else if operation == multiplicationSign then
        Multiplication

    else if operation == divisionSign then
        Division

    else
        Addition
