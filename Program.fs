open Initialization
open LocalSearch.Play
open LocalSearch.Scoring.Scorers
open LocalSearch.Selection
open Game
open RuntimeBenchmark
open Test
open Writer

open System

let usage = @"Usage: Run with one argument.
test will run unit tests
maximalScoreLocalSearch will play 2048 25 times using a local search that seeks maximum score
maximalBlanksLocalSearch will play 2048 25 times using a local search that seeks maximum number of blank spaces
maximalBlanksThenScoreLocalSearch will play 2048 25 times using a local search that seeks maximum number of blank spaces and score, in that order
maximalExpectedScoreLocalSearch will play 2048 25 times using a local search that seeks maximum expected score of each action"

let printUsage () =
    usage
    |> writeStringToConsole

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
    | [|"randomLocalSearch"; trials|] ->
        initialState
        |> playExpectimax chooseByRandomImprovement (int trials)
        ||> writeResult
    | [|"maximalScoreLocalSearch"; trials|] ->
        initialState
        |> playExpectimax chooseByBestScore (int trials)
        ||> writeResult
    | [|"maximalBlanksLocalSearch"; trials|] ->
        initialState
        |> playExpectimax chooseByMostOpenSpaces (int trials)
        ||> writeResult
    | [|"maximalBlanksThenScoreLocalSearch"; trials|] ->
        initialState
        |> playExpectimax chooseByMostOpenSpacesWithHighestScore (int trials)
        ||> writeResult
    | [|"maximalExpectedScoreLocalSearch"; trials|] ->
        initialState
        |> playExpectimax chooseByBestScoreExpectation (int trials)
        ||> writeResult
    | [|"EUMR"; trials|] ->
        initialState
        |> playExpectimax chooseByEUMR (int trials)
        ||> writeResult
    | [|"ABPrunedMaximumScore"; trials|] ->
        initialState
        |> playAlphaBeta scoreByScore (int trials)
        ||> writeResult
    | [|"MinimaxMaximumScore"; trials|] ->
        initialState
        |> playMinimax scoreByScore (int trials)
        ||> writeResult
    | _ ->
        printUsage ()

[<EntryPoint>]
let main args =
    try
        (commandSwitch, args) ||> measureAndPrintRuntime
    with
    | :? FormatException as fe ->
        "Error: " + fe.Message |> printfn "%s"
        printUsage ()
    0
