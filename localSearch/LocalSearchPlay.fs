module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.AlphaBetaPrunedMinimax
open LocalSearch.ExhaustiveSearch

let playTrialsWithExhaustiveSearch tileInsertionOptions depth evaluationFunction numTrials initialState =

    let searchFunction = exhaustiveSearch evaluationFunction tileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination searchFunction returnFromTerminalState

    localSearch
    |> Seq.replicate numTrials
    |> Seq.map ((|>) initialState)
    |> Seq.maxBy (fst >> GameState.scoreOf)

let playTrialsWithAlphaBetaPruning tileInsertionOptions depth scoringFunction numTrials initialState =

    let searchFunction = alphaBetaMinimaxSearch scoringFunction tileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination searchFunction returnFromTerminalState

    localSearch
    |> Seq.replicate numTrials
    |> Seq.map ((|>) initialState)
    |> Seq.maxBy (fst >> GameState.scoreOf)
