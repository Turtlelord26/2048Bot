open Initialization
open LocalSearch
open LocalSearchScoring
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
    match args with
    | [|"test"|] ->
        tests ()
    | [|"maximalScoreLocalSearch"|] ->
        initialState
        |> playTrials 2 chooseByBestScore 25
        ||> writeResult
    | [|"maximalBlanksLocalSearch"|] ->
        initialState
        |> playTrials 2 chooseByMostOpenSpaces 25
        ||> writeResult
    | [|"maximalBlanksThenScoreLocalSearch"|] ->
        initialState
        |> playTrials 2 chooseByMostOpenSpacesWithHighestScore 25
        ||> writeResult
    | [|"maximalExpectedScoreLocalSearch"|] ->
        initialState
        |> playTrials 2 chooseByBestScoreExpectation 25
        ||> writeResult
    | _ ->
        usage
        |> writeStringToConsole
    0
