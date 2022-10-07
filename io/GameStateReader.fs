module Reader

open Game

open System
open System.IO

let readGameStates =

    let toGameState tiles =
        State(tiles, 0)

    let toTile i =
        match i with
        | 0 -> Blank
        | _ -> i |> Math.Log2 |> int |> Exponent

    let readRow (row: string) =
        row.Split(',')
        |> Array.map int
        |> Array.map toTile

    let readState rows =
        let board = Array2D.create 4 4 Blank
        let tiles =
            rows
            |> Array.map readRow
        for i in 0..3 do
            board[i,0..3] <- tiles[i]
        board
    
    let readStates states =
        try
            states
            |> Seq.map readState
            |> Ok
        with
        | :? ArgumentException as e -> e.Message |> Error
        | :? FormatException as e -> e.Message |> Error
        | :? OverflowException as e -> e.Message |> Error
    
    let readLines path =
        try
            File.ReadLines(path) |> Ok
        with
        | :? ArgumentException as e -> e.Message |> Error
        | :? DirectoryNotFoundException as e -> e.Message |> Error
        | :? FileNotFoundException as e -> e.Message |> Error
        | :? PathTooLongException as e -> e.Message |> Error
        | :? IOException as e -> e.Message |> Error
        | :? Security.SecurityException as e -> e.Message |> Error
        | :? UnauthorizedAccessException as e -> e.Message |> Error

    readLines
    >> Result.map Seq.tail 
    >> Result.map (Seq.chunkBySize 4)
    >> Result.bind readStates
    >> Result.map (Seq.map toGameState)