module Array2DUtils

let private rangeUpTo exclusiveMax =
    [0..(exclusiveMax - 1)]

let rowIndices array =
    array
    |> Array2D.length1
    |> rangeUpTo

let colIndices array =
    array
    |> Array2D.length2
    |> rangeUpTo
