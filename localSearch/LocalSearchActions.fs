module LocalSearch.Actions

open Action
open Moves

let returnFromTerminalState state actions =
    state, actions |> Seq.rev

let searchActionsUntilTermination tileInsertionOptions determineAction returnFromTerminalState =

    let rec recurSearch actions state =
        match
            state
            |> determineAction
        with
        | Some Left ->
            state
            |> moveLeft tileInsertionOptions
            |> recurSearch (Left :: actions)
        | Some Right ->
            state
            |> moveRight tileInsertionOptions
            |> recurSearch (Right :: actions)
        | Some Up ->
            state
            |> moveUp tileInsertionOptions
            |> recurSearch (Up :: actions)
        | Some Down ->
            state
            |> moveDown tileInsertionOptions
            |> recurSearch (Down :: actions)
        | None ->
            returnFromTerminalState state actions
    
    recurSearch []
