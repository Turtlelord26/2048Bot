module TestStates

open Game

let testState = 
    let board = Array2D.create 4 4 Blank
    board[0,0] <- Exponent 1
    board[3,3] <- Exponent 2
    board[0,3] <- Exponent 3
    board[3,0] <- Exponent 4
    board[1,1] <- Exponent 1
    board[2,1] <- Exponent 1
    board[1,2] <- Exponent 1
    board[2,2] <- Exponent 1
    (board, 0)
    |> State

let expectedLeft = 
    let board = Array2D.create 4 4 Blank
    board[0,0] <- Exponent 1
    board[3,1] <- Exponent 2
    board[0,1] <- Exponent 3
    board[3,0] <- Exponent 4
    board[1,0] <- Exponent 2
    board[2,0] <- Exponent 2
    (board, 8)
    |> State

let expectedRight = 
    let board = Array2D.create 4 4 Blank
    board[0,2] <- Exponent 1
    board[3,3] <- Exponent 2
    board[0,3] <- Exponent 3
    board[3,2] <- Exponent 4
    board[1,3] <- Exponent 2
    board[2,3] <- Exponent 2
    (board, 8)
    |> State

let expectedUp = 
    let board = Array2D.create 4 4 Blank
    board[0,0] <- Exponent 1
    board[1,3] <- Exponent 2
    board[0,3] <- Exponent 3
    board[1,0] <- Exponent 4
    board[0,1] <- Exponent 2
    board[0,2] <- Exponent 2
    (board, 8)
    |> State

let expectedDown = 
    let board = Array2D.create 4 4 Blank
    board[2,0] <- Exponent 1
    board[3,3] <- Exponent 2
    board[2,3] <- Exponent 3
    board[3,0] <- Exponent 4
    board[3,1] <- Exponent 2
    board[3,2] <- Exponent 2
    (board, 8)
    |> State

let initialOrderMattersState =
    let board = Array2D.create 4 4 Blank
    board[0,0] <- Exponent 2
    board[0,1] <- Exponent 2
    board[0,2] <- Exponent 2
    board[2,1] <- Exponent 2
    board[2,2] <- Exponent 2
    board[2,3] <- Exponent 2
    (board, 0)
    |> State

let expectedOrderMattersState =
    let board = Array2D.create 4 4 Blank
    board[0,0..1] <- [|Exponent 3; Exponent 2|]
    board[2,0..1] <- [|Exponent 3; Exponent 2|]
    (board, 16)
    |> State

let expectedOrderMatters2State =
    let board = Array2D.create 4 4 Blank
    board[0,2..3] <- [|Exponent 2; Exponent 3|]
    board[2,2..3] <- [|Exponent 2; Exponent 3|]
    (board, 16)
    |> State


let initialPackedState =
    (Exponent 1 |> Array2D.create 4 4, 0)
    |> State

let expectedPackedState =
    let board = Array2D.create 4 4 Blank
    board[0,0] <- Exponent 2
    board[0,1] <- Exponent 2
    board[1,0] <- Exponent 2
    board[1,1] <- Exponent 2
    board[2,0] <- Exponent 2
    board[2,1] <- Exponent 2
    board[3,0] <- Exponent 2
    board[3,1] <- Exponent 2
    (board, 32)
    |> State

let expectedRightUpState =
    let board = Array2D.create 4 4 Blank
    board[0..2, 3] <- [|Exponent 3; Exponent 3; Exponent 2|]
    board[0..1, 2] <- [|Exponent 1; Exponent 4|]
    (board, 16)
    |> State

let initialPackedUnlikeState =
    let board = Array2D.create 4 4 Blank
    board[0,0..3] <- [|Exponent 3; Exponent 2; Exponent 2; Exponent 2|]
    (board, 2)
    |> State

let expectedPackedUnlikeState =
    let board = Array2D.create 4 4 Blank
    board[0,0..2] <- [|Exponent 3; Exponent 3; Exponent 2|]
    (board, 10)
    |> State

let milestone1SampleInitial =
    let board = Array2D.create 4 4 Blank
    board[0..3,0] <- [|Exponent 1; Exponent 4; Exponent 2; Exponent 1|]
    board[0..3,1] <- [|Exponent 2; Blank; Exponent 3; Exponent 3|]
    board[0,2] <- Exponent 2
    (board, 0)
    |> State

let milestone1SampleFirstExpected =
    let board = Array2D.create 4 4 Blank
    board[0..3,0] <- [|Exponent 1; Exponent 4; Exponent 2; Exponent 1|]
    board[0..3,1] <- [|Exponent 3; Blank; Exponent 3; Exponent 3|]
    (board, 8)
    |> State


let milestone1SampleSecondExpected =
    let board = Array2D.create 4 4 Blank
    board[0..3,0] <- [|Exponent 1; Exponent 4; Exponent 2; Exponent 1|]
    board[0..3,1] <- [|Blank; Blank; Exponent 3; Exponent 4|]
    (board, 24)
    |> State
