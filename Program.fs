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

let tileInsertionOptions = [Exponent 1; Exponent 2]

let playExpectimax = playTrialsWithExhaustiveSearch tileInsertionOptions

let playAlphaBeta = playTrialsWithAlphaBetaPruning tileInsertionOptions

let playMinimax = playTrialsWithMinimax tileInsertionOptions

let initialTileOptions = [Exponent 1]

let initialState = makeInitialBoard 4 4 2 initialTileOptions

let commandSwitch args =
    match args with
    | [|"test"|] ->
        tests ()
    | [|"ExpectimaxMaximumScore"; lookaheads; trials|] ->
        initialState
        |> playExpectimax (int lookaheads) chooseByBestScoreExpectation (int trials)
        ||> writeResult
    | [|"ExpectimaxEUMR"; lookaheads; trials|] ->
        initialState
        |> playExpectimax (int lookaheads) chooseByEUMR (int trials)
        ||> writeResult
    | [|"ABPrunedMaximumScore"; lookaheads; trials|] ->
        initialState
        |> playAlphaBeta (int lookaheads) scoreByScore (int trials)
        ||> writeResult
    | [|"MinimaxMaximumScore"; lookaheads; trials|] ->
        initialState
        |> playMinimax (int lookaheads) scoreByScore (int trials)
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
