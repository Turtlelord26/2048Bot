open Initialization
open LocalSearch.Play
open LocalSearch.Scoring.Scorers
open LocalSearch.Scoring.CompositeEvaluators
open Game
open RuntimeBenchmark
open Test
open Writer

open System

let usage = @"
Usage: dotnet run [keyword] [lookahead depth] [trials].
Keywords:
MinimaxMaximumScore will play 2048 using a minimax search that uses the ingame score of states
ABPrunedMinimaxMaximumScore will play 2048 using an alpha-beta pruned minimax search, using the ingame scores of states.
ExpectimaxMaximumScore will play 2048 25 times using an expectimax search that uses ingame scores
ExpectimaxEUMR will play 2048 using expectimax search and a multistage heuristic to evaluate states

Lookahead depth is the depth of search trees generated when calculating the next move. Must be >=2. 2 is the most well-tested. 3 and above run slowly.

Trials is the number of games the program will run back to back. Only the highest-scoring game will be printed to console. Must be >=1.

To run unit tests: dotnet run test
"

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
