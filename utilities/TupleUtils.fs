module TupleUtils

let mapFst mapper (first, second) =
    (first |> mapper, second)

let mapSnd mapper (first, second) =
    (first, second |> mapper)
