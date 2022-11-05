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
    testRead false

[<EntryPoint>]
let main args =
    match args with
    | [|"test"|] ->
        tests ()
    | [|"randomLocalSearch"|] ->
        initialState
        |> playTrialsWithRandomLocalSearch 100 2
        ||> writeResult
    | [|"maximalScoreLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalLocalSearch 25 2
        ||> writeResult
    | [|"maximalBlanksLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalBlanksLocalSearch 25 2
        ||> writeResult
    | [|"maximalBlanksThenScoreLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalBlanksThenScoreLocalSearch 25 2
        ||> writeResult
    | [|"maximalExpectedScoreLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalScoreExpectationLocalSearch 25 2
        ||> writeResult
    | _ ->
        usage
        |> writeStringToConsole
    0
