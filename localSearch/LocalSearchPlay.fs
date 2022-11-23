module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.Search

let private bestTrialOfPlayWithSearch searchFunction returnFromTerminalState numTrials initialState =

    let localSearch = searchActionsUntilTermination searchFunction returnFromTerminalState

    localSearch
    |> Seq.replicate numTrials
    |> Seq.map ((|>) initialState)
    |> Seq.maxBy (fst >> GameState.scoreOf)

let playTrials tileInsertionOptions depth scoringFunction trials initialState =

    let searchFunction =
        nthChildren tileInsertionOptions depth
        >> scoringFunction
    
    bestTrialOfPlayWithSearch searchFunction returnFromTerminalState trials initialState
