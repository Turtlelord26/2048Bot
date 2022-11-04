module Moves

open Game

let private tileInsertionOptions =
    seq {Exponent 1; Exponent 2}

let private insertRandomTile = 
    tileInsertionOptions
    |> GameState.addRandomTile

let moveLeft =
    GameState.shiftLeft
    >> insertRandomTile

let moveRight =
    GameState.shiftRight
    >> insertRandomTile

let moveUp =
    GameState.shiftUp
    >> insertRandomTile

let moveDown =
    GameState.shiftDown
    >> insertRandomTile
