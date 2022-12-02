module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.AlphaBetaPrunedMinimax
open LocalSearch.ExhaustiveSearch
open LocalSearch.Minimax

let private unweight tileInsertionOptions =
        Seq.map snd tileInsertionOptions

let playTrialsWithExhaustiveSearch weightedTileInsertionOptions depth evaluationFunction numTrials initialState =

    let unweightedTileInsertionOptions = unweight weightedTileInsertionOptions

    let searchFunction = exhaustiveSearch evaluationFunction unweightedTileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination weightedTileInsertionOptions searchFunction returnFromTerminalState

    localSearch
    |> Seq.replicate numTrials
    |> Seq.map ((|>) initialState)
    |> Seq.maxBy (fst >> GameState.scoreOf)

let playTrialsWithAlphaBetaPruning weightedTileInsertionOptions depth scoringFunction numTrials initialState =

    let unweightedTileInsertionOptions = unweight weightedTileInsertionOptions

    let searchFunction = alphaBetaMinimaxSearch scoringFunction unweightedTileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination weightedTileInsertionOptions searchFunction returnFromTerminalState

    localSearch
    |> Seq.replicate numTrials
    |> Seq.map ((|>) initialState)
    |> Seq.maxBy (fst >> GameState.scoreOf)

let playTrialsWithMinimax weightedTileInsertionOptions depth scoringFunction numTrials initialState =

    let unweightedTileInsertionOptions = unweight weightedTileInsertionOptions

    let searchFunction = minimaxSearch scoringFunction unweightedTileInsertionOptions depth
    
    let localSearch = searchActionsUntilTermination weightedTileInsertionOptions searchFunction returnFromTerminalState

    localSearch
    |> Seq.replicate numTrials
    |> Seq.map ((|>) initialState)
    |> Seq.maxBy (fst >> GameState.scoreOf)
