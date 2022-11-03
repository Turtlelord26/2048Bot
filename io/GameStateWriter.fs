module Writer

open System
open System.IO
open System.Security

open Array2DUtils
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

let printGameState (State (board, score)) =

    let printTile tile =
        match tile |> Tile.score with
        | i when i > 0 -> i |> sprintf "%-6d"
        | _ -> sprintf "%6s" ""
    
    let encloseWithPipe str =
        "| " + str + " |"

    let printRow =
        Seq.map printTile
        >> Seq.reduce concatWithSpacedPipe
        >> encloseWithPipe
    
    let spacer =
        seq {for _ in rowIndices board do "---------"}
        |> Seq.fold (+) "-"
    
    let printSlice (board: Tile[,]) row =
        printRow board[row,*]
        |> sprintf "%s"
    
    let printBoard board =
        board
        |> rowIndices
        |> List.map (printSlice board)
        |> List.reduce (concatWithDelimiter $"\n{spacer}\n")
        |> sprintf "%s"

    [sprintf "%s" spacer;
        printBoard board;
        sprintf "%s" spacer;
        sprintf "%s" $"score: {score}";]
    |> List.reduce concatWithNewline

let writeResult state actions =
    
    let writeActions actions =

        let numActionsStr =
            actions
            |> Seq.length
            |> sprintf "%d"
            |> (+) "Moves taken: "

        actions
        |> Action.manyToString
        |> concatWithNewline numActionsStr
    
    let printStateAndActions =
        printGameState
        >> concatWithNewline (writeActions actions)
    
    try
        state
        |> printStateAndActions
        |> printfn "%s"
    with
    | :? ArgumentException as e -> e.Message |> printfn "%s"
    | :? PathTooLongException as e -> e.Message |> printfn "%s"
    | :? DirectoryNotFoundException as e -> e.Message |> printfn "%s"
    | :? IOException as e -> e.Message |> printfn "%s"
    | :? UnauthorizedAccessException as e -> e.Message |> printfn "%s"
    | :? NotSupportedException as e -> e.Message |> printfn "%s"
    | :? SecurityException as e -> e.Message |> printfn "%s"
