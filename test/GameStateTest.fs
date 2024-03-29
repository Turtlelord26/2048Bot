module Test

open Game
open GameStateTestUtils
open TestStates
open TestStateExpectations

let testLeftShift  =
    testShift "Test left shift: " GameState.shiftLeft testState expectedLeft
    
let testRightShift =
    testShift "Test right shift: " GameState.shiftRight testState expectedRight
    
let testUpShift =
    testShift "Test up shift: " GameState.shiftUp testState expectedUp
    
let testDownShift =
    testShift "Test down shift: " GameState.shiftDown testState expectedDown
    
let testBasicShifts =
    testLeftShift
    testRightShift
    testUpShift
    testDownShift

let testOrderMatters =
    testShift "Test order matters shift: " GameState.shiftLeft initialOrderMattersState expectedOrderMattersState
    
let testOrderMatters2 =
    testShift "Test order matters shift: " GameState.shiftRight initialOrderMattersState expectedOrderMatters2State
    
let testPackedBoard =
    testShift "Test packed board: " GameState.shiftLeft initialPackedState expectedPackedState
    
let testRightUp =
    testShift "RightThenUp: " (GameState.shiftRight >> GameState.shiftUp) testState expectedRightUpState
    
let testUp2 =
    testShift "TestUp2: " (GameState.shiftRight >> GameState.shiftUp) testState expectedRightUpState
    
let testUnlikePacked =
    testShift "TestUnlikePackedRow: " GameState.shiftLeft initialPackedUnlikeState expectedPackedUnlikeState
    
let testMilestone1SampleFirstMove =
    testShift "TestMilestone1Sample1st: " GameState.shiftLeft milestone1SampleInitial milestone1SampleFirstExpected
    
let testMilestone1SampleSecondMove =
    testShift "TestMilestone1Sample2nd: " (GameState.shiftLeft >> GameState.shiftDown) milestone1SampleInitial milestone1SampleSecondExpected
