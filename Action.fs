namespace Action

open StringUtils

type Action =
    | Left
    | Right
    | Up
    | Down
    with

    static member toString action =
        match action with
        | Left -> "L"
        | Right -> "R"
        | Up -> "U"
        | Down -> "D"

    static member manyToString =
        Seq.map Action.toString
        >> Seq.reduce concatWithComma
