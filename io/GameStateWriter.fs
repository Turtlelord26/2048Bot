module Writer

open System
open System.IO
open System.Security

open Game
open Search
open StringUtils

let writeSolutions path solutions =
    try
        File.WriteAllText(path, solutions)
    with
    | :? ArgumentException as e -> e.Message |> printfn "%s"
    | :? PathTooLongException as e -> e.Message |> printfn "%s"
    | :? DirectoryNotFoundException as e -> e.Message |> printfn "%s"
    | :? IOException as e -> e.Message |> printfn "%s"
    | :? UnauthorizedAccessException as e -> e.Message |> printfn "%s"
    | :? NotSupportedException as e -> e.Message |> printfn "%s"
    | :? SecurityException as e -> e.Message |> printfn "%s"

let writeResult state actions =

    let writeTile tile =
        match tile with
        | Exponent value ->
            value
            |> Tile.scoreValue
            |> sprintf "%d"
        | Blank ->
            "0"

    let writeTiles tiles =
        seq {for i in 0..(Array2D.length1 tiles - 1) do tiles[i,*]}
        |> Seq.map (Seq.map writeTile)
        |> Seq.map (Seq.reduce concatWithComma)
        |> Seq.reduce concatWithNewline
    
    let writeScore =
        sprintf "%d"
        >> (+) "Score: "
    
    let writeState (State (tiles, score)) =
        writeTiles tiles
        |> concatWithNewline (writeScore score)
    
    let writeActions actions =
        actions
        |> Action.manyToString
        |> concatWithNewline (Seq.length actions |> sprintf "%d" |> (+) "Moves taken: ")
    
    let outString =
        writeState state
        |> concatWithNewline (writeActions actions)
    
    try
        outString
        |> printfn "%s"
    with
    | :? ArgumentException as e -> e.Message |> printfn "%s"
    | :? PathTooLongException as e -> e.Message |> printfn "%s"
    | :? DirectoryNotFoundException as e -> e.Message |> printfn "%s"
    | :? IOException as e -> e.Message |> printfn "%s"
    | :? UnauthorizedAccessException as e -> e.Message |> printfn "%s"
    | :? NotSupportedException as e -> e.Message |> printfn "%s"
    | :? SecurityException as e -> e.Message |> printfn "%s"
