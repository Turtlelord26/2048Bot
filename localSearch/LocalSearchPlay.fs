module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.Search

let private bestTrialOfPlayWithSearch searchFunction returnFromTerminalState numTrials initialState =

    let localSearch = searchActionsUntilTermination searchFunction returnFromTerminalState

    seq {1..numTrials}
    |> Seq.map (fun _ -> localSearch initialState)
    |> Seq.maxBy (fst >> GameState.scoreOf)

let playTrials tileInsertionOptions depth scoringFunction trials initialState =

    let searchFunction =
        nthChildren tileInsertionOptions depth
        >> scoringFunction
    
    bestTrialOfPlayWithSearch searchFunction returnFromTerminalState trials initialState
