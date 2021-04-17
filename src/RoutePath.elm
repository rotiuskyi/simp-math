module RoutePath exposing (RoutePath(..), fromString, toString)


type RoutePath
    = Root
    | NotFound
    | Login


fromString : String -> RoutePath
fromString path =
    case path of
        "/" ->
            Root

        "/login" ->
            Login

        _ ->
            NotFound


toString : RoutePath -> String
toString path =
    case path of
        Root ->
            "/"

        Login ->
            "/login"

        NotFound ->
            "/not_found"
