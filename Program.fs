open Initialization
open LocalSearch.Play
open LocalSearch.Scoring.Scorers
open LocalSearch.Selection
open Game
open RuntimeBenchmark
open Test
open Writer

let usage = @"Usage: Run with one argument.
test will run unit tests
maximalScoreLocalSearch will play 2048 25 times using a local search that seeks maximum score
maximalBlanksLocalSearch will play 2048 25 times using a local search that seeks maximum number of blank spaces
maximalBlanksThenScoreLocalSearch will play 2048 25 times using a local search that seeks maximum number of blank spaces and score, in that order
maximalExpectedScoreLocalSearch will play 2048 25 times using a local search that seeks maximum expected score of each action"

let tests () =
    testBasicShifts
    testOrderMatters
    testOrderMatters2
    testPackedBoard
    testRightUp
    testUp2
    testUnlikePacked
    testMilestone1SampleFirstMove
    testMilestone1SampleSecondMove

let lookaheads = 2

let tileInsertionOptions = seq {0.5, Exponent 1; 0.5, Exponent 2}

let playExpectimax = playTrialsWithExhaustiveSearch tileInsertionOptions lookaheads

let playAlphaBeta = playTrialsWithAlphaBetaPruning tileInsertionOptions lookaheads

let playMinimax = playTrialsWithMinimax tileInsertionOptions lookaheads

let initialTileOptions = seq {1., Exponent 1}

let initialState = makeInitialBoard 4 4 2 initialTileOptions

let commandSwitch args =
    match args with
    | [|"test"|] ->
        tests ()
    | [|"randomLocalSearch"|] ->
        initialState
        |> playExpectimax chooseByRandomImprovement 1
        ||> writeResult
    | [|"maximalScoreLocalSearch"|] ->
        initialState
        |> playExpectimax chooseByBestScore 1
        ||> writeResult
    | [|"maximalBlanksLocalSearch"|] ->
        initialState
        |> playExpectimax chooseByMostOpenSpaces 1
        ||> writeResult
    | [|"maximalBlanksThenScoreLocalSearch"|] ->
        initialState
        |> playExpectimax chooseByMostOpenSpacesWithHighestScore 1
        ||> writeResult
    | [|"maximalExpectedScoreLocalSearch"|] ->
        initialState
        |> playExpectimax chooseByBestScoreExpectation 1
        ||> writeResult
    | [|"EUMR"|] ->
        initialState
        |> playExpectimax chooseByEUMR 1
        ||> writeResult
    | [|"ABPrunedMaximumScore"|] ->
        initialState
        |> playAlphaBeta scoreByScore 1
        ||> writeResult
    | [|"MinimaxMaximumScore"|] ->
        initialState
        |> playMinimax scoreByScore 1
        ||> writeResult
    | _ ->
        usage
        |> writeStringToConsole

[<EntryPoint>]
let main args =
    (commandSwitch, args) ||> measureAndPrintRuntime
    0
