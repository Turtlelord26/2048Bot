module TestUtils

open Game
open StringUtils

let passfail assertion =
    match assertion with
    | true -> "Pass"
    | false -> "Fail"

let printGameState (gameState: GameState) =

    let printTile tile =
        match tile with
        | Exponent value -> value |> Tile.scoreValue |> sprintf "%-6d"
        | Blank -> sprintf "%6s" ""
    
    let encloseWithPipe str =
        "| " + str + " |"

    let printRow =
        Seq.map printTile
        >> Seq.reduce concatWithSpacedPipe
        >> encloseWithPipe
    
    let spacer =
        seq {for _ in 0..gameState.maxRow do "---------"}
        |> Seq.fold (+) "-"
    
    let printSlice (state: Tile[,]) row =
        printRow state[row,*]
        |> sprintf "%s"

    let (State (state, score)) = gameState
    printfn "%s" spacer
    [0..gameState.maxRow]
    |> List.map (printSlice state)
    |> List.reduce (concatWithDelimiter $"\n{spacer}\n")
    |> printfn "%s"
    printfn "%s" spacer
    printfn "%s" $"score: {score}"

let testShift visualize preface shiftfn initialState expectation =

    let actualState =
        initialState
        |> shiftfn
    
    let printState label state =
        printfn "%s" label
        state
        |> printGameState

    let printStates () =
        printfn "%s" preface
        printState "Initial State:" initialState
        printState "After Shift:" actualState
        printState "Expected:" expectation
    
    if visualize then printStates ()

    let assertEqual expected actual =
        expected = actual

    actualState
    |> assertEqual expectation
    |> passfail
    |> (+) preface
    |> printfn "%s"
