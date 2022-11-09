module LocalSearchActions

open Game
open Moves
open Search

let returnFromTerminalState vstate actions =
    vstate, actions |> Seq.rev

let private searchNextActionFromValidState determineAction recurSearch vstate actions =
    match vstate |> determineAction with
    | Some Left ->
        vstate
        |> ValidatedGameState.map moveLeft
        |> recurSearch (Left :: actions)
    | Some Right ->
        vstate
        |> ValidatedGameState.map moveRight
        |> recurSearch (Right :: actions)
    | Some Up ->
        vstate
        |> ValidatedGameState.map moveUp
        |> recurSearch (Up :: actions)
    | Some Down ->
        vstate
        |> ValidatedGameState.map moveDown
        |> recurSearch (Down :: actions)
    | None ->
        returnFromTerminalState vstate actions

let searchAndExecuteNextAction search =
        ValidatedGameState.stateOf >> search
        |> searchNextActionFromValidState
