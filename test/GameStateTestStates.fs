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

let initialPackedState =
    (Exponent 1 |> Array2D.create 4 4, 0)
    |> State

let initialPackedUnlikeState =
    let board = Array2D.create 4 4 Blank
    board[0,0..3] <- [|Exponent 3; Exponent 2; Exponent 2; Exponent 2|]
    (board, 2)
    |> State

let milestone1SampleInitial =
    let board = Array2D.create 4 4 Blank
    board[0..3,0] <- [|Exponent 1; Exponent 4; Exponent 2; Exponent 1|]
    board[0..3,1] <- [|Exponent 2; Blank; Exponent 3; Exponent 3|]
    board[0,2] <- Exponent 2
    (board, 0)
    |> State
