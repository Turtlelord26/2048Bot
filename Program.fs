open Initialization
open LocalSearch.Play
open LocalSearch.Scoring.Scorers
open LocalSearch.Selection
open Moves
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

[<EntryPoint>]
let main args =

    let lookaheads = 2

    let play = playTrialsWithExhaustiveSearch tileInsertionOptions lookaheads

    let playAB = playTrialsWithAlphaBetaPruning tileInsertionOptions lookaheads

    match args with
    | [|"test"|] ->
        tests ()
    | [|"randomLocalSearch"|] ->
        initialState
        |> play chooseByRandomImprovement 1
        ||> writeResult
    | [|"maximalScoreLocalSearch"|] ->
        initialState
        |> play chooseByBestScore 1
        ||> writeResult
    | [|"maximalBlanksLocalSearch"|] ->
        initialState
        |> play chooseByMostOpenSpaces 1
        ||> writeResult
    | [|"maximalBlanksThenScoreLocalSearch"|] ->
        initialState
        |> play chooseByMostOpenSpacesWithHighestScore 1
        ||> writeResult
    | [|"maximalExpectedScoreLocalSearch"|] ->
        initialState
        |> play chooseByBestScoreExpectation 1
        ||> writeResult
    | [|"EUMR"|] ->
        initialState
        |> play chooseByEUMR 1
        ||> writeResult
    | [|"ABPrunedMaximumScore"|] ->
        initialState
        |> playAB scoreByScore 1
        ||> writeResult
    | _ ->
        usage
        |> writeStringToConsole
    0
