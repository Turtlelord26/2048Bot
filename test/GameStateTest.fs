module Test

open Game
open TestUtils
open TestStates
open Reader

let testLeftShift visualize =
    testShift visualize "Test left shift: " GameState.shiftLeft testState expectedLeft
    
let testRightShift visualize =
    testShift visualize "Test right shift: " GameState.shiftRight testState expectedRight
    
let testUpShift visualize =
    testShift visualize "Test up shift: " GameState.shiftUp testState expectedUp
    
let testDownShift visualize =
    testShift visualize "Test down shift: " GameState.shiftDown testState expectedDown
    
let testBasicShifts visLeft visRight visUp visDown =
    testLeftShift visLeft
    testRightShift visRight
    testUpShift visUp
    testDownShift visDown

let testOrderMatters visualize =
    testShift visualize "Test order matters shift: " GameState.shiftLeft initialOrderMattersState expectedOrderMattersState
    
let testOrderMatters2 visualize =
    testShift visualize "Test order matters shift: " GameState.shiftRight initialOrderMattersState expectedOrderMatters2State
    
let testPackedBoard visualize =
    testShift visualize "Test packed board: " GameState.shiftLeft initialPackedState expectedPackedState
    
let testRightUp visualize =
    testShift visualize "RightThenUp: " (GameState.shiftRight >> GameState.shiftUp) testState expectedRightUpState
    
let testUp2 visualize =
    testShift visualize "TestUp2: " GameState.shiftUp expectedRight expectedRightUpState
    
let testUnlikePacked visualize =
    testShift visualize "TestUnlikePackedRow: " GameState.shiftLeft initialPackedUnlikeState expectedPackedUnlikeState
    
let testMilestone1SampleFirstMove visualize =
    testShift visualize "TestMilestone1Sample1st: " GameState.shiftLeft milestone1SampleInitial milestone1SampleFirstExpected
    
let testMilestone1SampleSecondMove visualize =
    testShift visualize "TestMilestone1Sample2nd: " (GameState.shiftLeft >> GameState.shiftDown) milestone1SampleInitial milestone1SampleSecondExpected
    
let testRead visualize =
    match 
        "test/unitTestInput.txt"
        |> readGameStates
        |> Result.map Seq.head
    with
    | Ok state ->
        $"Test Input Reading: {state = milestone1SampleInitial |> passfail}" |> printfn "%s"
        if visualize then state |> printGameState
    | Error error -> error |> printfn "%s"
