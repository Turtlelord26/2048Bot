open Reader
open Writer
open Milestone1
open StringUtils
open Test

let tests () =
    testBasicShifts false false false false
    testOrderMatters false
    testOrderMatters2 false
    testPackedBoard false
    testRightUp false
    testUp2 false
    testUnlikePacked false
    testMilestone1SampleFirstMove false
    testMilestone1SampleSecondMove false
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

let milestone1Args args =
    match args with
    | [|_; inPath; outPath|] -> inPath |> milestone1 outPath
    | _ -> printfn "%s" "Milestone 1 usage: Program.fs 2048_in.txt 2048_out.txt"

[<EntryPoint>]
let main args =
    //tests ()
    milestone1Args args
    0