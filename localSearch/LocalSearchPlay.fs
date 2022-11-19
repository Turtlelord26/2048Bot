module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.Search

let private bestTrialOfPlayWithSearch searchFunction returnFromTerminalState numTrials initialState =

    let localSearch = searchActionsUntilTermination searchFunction returnFromTerminalState

    let higherScoringState (state1, actions1) (state2, actions2) =
        if
            state1 |> GameState.scoreOf
            >= (state2 |> GameState.scoreOf)
        then (state1, actions1)
        else (state2, actions2)

    let rec runTrials count highestScoringState =
        match count with
        | i when i > 0 ->
            initialState
            |> localSearch
            |> higherScoringState highestScoringState
            |> runTrials (count - 1)
        | _ ->
            highestScoringState
    
    initialState
    |> localSearch
    |> runTrials (numTrials - 1)

let playTrials tileInsertionOptions depth scoringFunction trials initialState =

    let searchFunction =
        nthChildren tileInsertionOptions depth
        >> scoringFunction
    
    bestTrialOfPlayWithSearch searchFunction returnFromTerminalState trials initialState
