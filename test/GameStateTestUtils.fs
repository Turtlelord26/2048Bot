module GameStateTestUtils

open TestUtils
open Writer

let testShift preface shiftfn initialState testResult =
    
    let printState label state =
        printfn "%s" label
        state
        |> Game.ValidatedGameState.wrap
        |> printGameState

    let printStates preface initialState actualState =
        preface |> printfn "%s"
        printState "Initial State:" initialState |> printfn "%s"
        printState "After Shift:" actualState |> printfn "%s"
    
    executeTest preface initialState shiftfn testResult printStates
