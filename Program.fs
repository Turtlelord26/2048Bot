open Reader
open Writer
open Milestone1
open StringUtils
open Test

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

let milestone1 outPath inPath =

    let extractString result =
        match result with
        | Ok string ->
            string
            |> writeSolutions outPath;
            "Done" |> printfn "%s"
        | Error string ->
            string |> printfn "%s"

    inPath
    |> readGameStates
    |> Result.map (Seq.map comprehensiveStateSearchWithDeterministicTileAddition)
    |> Result.map (Seq.reduce concatWithNewline)
    |> extractString

[<EntryPoint>]
let main args =
    tests ()
    0