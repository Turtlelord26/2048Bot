module LocalSearch.Actions

open Action
open Moves
open SearchTree

let returnFromTerminalState state actions =
    state, actions |> Seq.rev

let searchActionsUntilTermination tileInsertionOptions determineAction returnFromTerminalState tree =

    let move action =
        match action with
        | Left -> moveLeft
        | Right -> moveRight
        | Up -> moveUp
        | Down -> moveDown
        <| tileInsertionOptions

    let rec determineNextActionOrReturnIfNone actions tree =
        match
            tree |> determineAction
        with
        | Some action, possibleChildren ->
            executeActionAndRecurSearch actions action tree possibleChildren
        | None, _ ->
            returnFromTerminalState (tree |> SearchTree.getState) actions

    and executeActionAndRecurSearch actions action tree possibleChildren =
        let nextState =
            tree
            |> SearchTree.mapState (move action)
        
        possibleChildren
        |> Seq.find (SearchTree.getState >> ((=) nextState))
        |> SearchTree.mapState SearchTree.toSearchTreeRoot
        |> determineNextActionOrReturnIfNone (action :: actions)
    
    determineNextActionOrReturnIfNone [] tree
