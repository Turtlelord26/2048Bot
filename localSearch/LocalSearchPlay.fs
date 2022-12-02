module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.AlphaBetaPrunedMinimax
open LocalSearch.ExhaustiveSearch
open LocalSearch.Minimax

let private unweight tileInsertionOptions =
        Seq.map snd tileInsertionOptions

let private runSearch numTrials initialState =
    Seq.replicate numTrials
    >> Seq.map ((|>) initialState)
    >> Seq.maxBy (fst >> GameState.scoreOf)

let playTrialsWithExhaustiveSearch weightedTileInsertionOptions depth evaluationFunction numTrials initialState =

    let unweightedTileInsertionOptions = unweight weightedTileInsertionOptions

    let searchFunction = exhaustiveSearch evaluationFunction unweightedTileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination weightedTileInsertionOptions searchFunction returnFromTerminalState

    localSearch
    |> runSearch numTrials initialState

let private playWithMinimaxSearch minimaxSearch weightedTileInsertionOptions depth scoringFunction numTrials initialState =

    let unweightedTileInsertionOptions = unweight weightedTileInsertionOptions

    let searchFunction = minimaxSearch scoringFunction unweightedTileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination weightedTileInsertionOptions searchFunction returnFromTerminalState

    localSearch
    |> runSearch numTrials initialState

let playTrialsWithAlphaBetaPruning weightedTileInsertionOptions = playWithMinimaxSearch alphaBetaMinimaxSearch weightedTileInsertionOptions

let playTrialsWithMinimax weightedTileInsertionOptions = playWithMinimaxSearch minimaxSearch weightedTileInsertionOptions
