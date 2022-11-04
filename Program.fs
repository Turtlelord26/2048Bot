open Initialization
open LocalSearch
open Test
open Writer

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
    | [||] ->
        tests ()
        0
    | [|"randomLocalSearch"|] ->
        initialState
        |> playTrialsWithRandomLocalSearch 100 2
        ||> writeResult
        0
    | [|"maximalLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalLocalSearch 25 2
        ||> writeResult
        0
    | [|"maximalBlanksLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalBlanksLocalSearch 25 2
        ||> writeResult
        0
    | [|"maximalBlanksThenScoreLocalSearch"|] ->
        initialState
        |> playTrialsWithMaximalBlanksThenScoreLocalSearch 25 2
        ||> writeResult
        0
    | _ ->
        1
