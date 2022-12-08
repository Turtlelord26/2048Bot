module TestStateExpectations

open Game

let expectedLeft (State (tiles, score)) = 
    tiles[0,0] = Exponent 1
    && tiles[3,1] = Exponent 2
    && tiles[0,1] = Exponent 3
    && tiles[3,0] = Exponent 4
    && tiles[1,0] = Exponent 2
    && tiles[2,0] = Exponent 2
    && score = 8

let expectedRight (State (tiles, score)) =
    tiles[0,2] = Exponent 1
    && tiles[3,3] = Exponent 2
    && tiles[0,3] = Exponent 3
    && tiles[3,2] = Exponent 4
    && tiles[1,3] = Exponent 2
    && tiles[2,3] = Exponent 2
    && score = 8

let expectedUp (State (tiles, score)) =
    tiles[0,0] = Exponent 1
    && tiles[1,3] = Exponent 2
    && tiles[0,3] = Exponent 3
    && tiles[1,0] = Exponent 4
    && tiles[0,1] = Exponent 2
    && tiles[0,2] = Exponent 2
    && score = 8

let expectedDown (State (tiles, score)) =
    tiles[2,0] = Exponent 1
    && tiles[3,3] = Exponent 2
    && tiles[2,3] = Exponent 3
    && tiles[3,0] = Exponent 4
    && tiles[3,1] = Exponent 2
    && tiles[3,2] = Exponent 2
    && score = 8

let expectedOrderMattersState (State (tiles, score)) =
    tiles[0,0..1] = [|Exponent 3; Exponent 2|]
    && tiles[2,0..1] = [|Exponent 3; Exponent 2|]
    && score = 16

let expectedOrderMatters2State (State (tiles, score)) =
    tiles[0,2..3] = [|Exponent 2; Exponent 3|]
    && tiles[2,2..3] = [|Exponent 2; Exponent 3|]
    && score = 16

let expectedPackedState (State (tiles, score)) =
    tiles[0,0] = Exponent 2
    && tiles[0,1] = Exponent 2
    && tiles[1,0] = Exponent 2
    && tiles[1,1] = Exponent 2
    && tiles[2,0] = Exponent 2
    && tiles[2,1] = Exponent 2
    && tiles[3,0] = Exponent 2
    && tiles[3,1] = Exponent 2
    && score = 32

let expectedRightUpState (State (tiles, score)) =
    tiles[0..2, 3] = [|Exponent 3; Exponent 3; Exponent 2|]
    && tiles[0..1, 2] = [|Exponent 1; Exponent 4|]
    && score = 16

let expectedPackedUnlikeState (State (tiles, score)) =
    tiles[0,0..2] = [|Exponent 3; Exponent 3; Exponent 2|]
    && score = 10

let milestone1SampleFirstExpected (State (tiles, score)) =
    tiles[0..3,0] = [|Exponent 1; Exponent 4; Exponent 2; Exponent 1|]
    && tiles[0,1] = Exponent 3
    && tiles[2..3,1] = [|Exponent 3; Exponent 3|]
    && score = 8

let milestone1SampleSecondExpected (State (tiles, score)) =
    tiles[0..3,0] = [|Exponent 1; Exponent 4; Exponent 2; Exponent 1|]
    && tiles[2..3,1] = [|Exponent 3; Exponent 4|]
    && score = 24
