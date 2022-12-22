module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.AlphaBetaPrunedMinimax
open LocalSearch.ExhaustiveSearch

let private runSearch numTrials initialState =
    Seq.replicate numTrials
    >> Seq.map ((|>) initialState)
    >> Seq.maxBy (fst >> GameState.scoreOf)

let playTrialsWithExhaustiveSearch tileInsertionOptions depth evaluationFunction numTrials initialState =

    let searchFunction = exhaustiveSearch evaluationFunction tileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination tileInsertionOptions searchFunction returnFromTerminalState

    localSearch
    |> runSearch numTrials initialState

let playTrialsWithAlphaBetaPruning tileInsertionOptions depth scoringFunction numTrials initialState =

    let searchFunction = alphaBetaMinimaxSearch scoringFunction tileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination tileInsertionOptions searchFunction returnFromTerminalState

    localSearch
    |> runSearch numTrials initialState
