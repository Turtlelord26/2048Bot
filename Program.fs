open Initialization
open LocalSearch
open Test
open Writer

let usage = @"Usage: Run with one argument.
test will run unit tests
randomLocalSearch will play 2048 100 times using a random local search
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
    match args with
    | [|"test"|] ->
        tests ()
    | [|"randomLocalSearch"|] ->
        initialState
        |> playTrialsWithRandomLocalSearch 2 100
        ||> writeResult
    | [|"maximalScoreLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalLocalSearch 2 25
        ||> writeResult
    | [|"maximalBlanksLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalBlanksLocalSearch 2 25
        ||> writeResult
    | [|"maximalBlanksThenScoreLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalBlanksThenScoreLocalSearch 2 25
        ||> writeResult
    | [|"maximalExpectedScoreLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalScoreExpectationLocalSearch 2 25
        ||> writeResult
    | _ ->
        usage
        |> writeStringToConsole
    0
