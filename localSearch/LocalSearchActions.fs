module LocalSearch.Actions

open Moves
open SearchTree

let returnFromTerminalState state actions =
    state, actions |> Seq.rev

let searchActionsUntilTermination determineAction returnFromTerminalState =

    let rec recurSearch actions state =
        match
            state
            |> determineAction
        with
        | Some Left ->
            state
            |> moveLeft
            |> recurSearch (Left :: actions)
        | Some Right ->
            state
            |> moveRight
            |> recurSearch (Right :: actions)
        | Some Up ->
            state
            |> moveUp
            |> recurSearch (Up :: actions)
        | Some Down ->
            state
            |> moveDown
            |> recurSearch (Down :: actions)
        | None ->
            returnFromTerminalState state actions
    
    recurSearch []
