module GameStateTestUtils

open Game
open StringUtils
open TestUtils

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

let testShift preface shiftfn initialState testResult =
    
    let printState label state =
        printfn "%s" label
        state
        |> printGameState

    let printStates preface initialState actualState =
        printfn "%s" preface
        printState "Initial State:" initialState
        printState "After Shift:" actualState
    
    executeTest preface initialState shiftfn testResult printStates
