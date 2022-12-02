module Moves

open Game

let private move shiftFn tileInsertionOptions = 
    shiftFn
    >> GameState.addRandomTile tileInsertionOptions

let moveLeft tileInsertionOptions = 
    move GameState.shiftLeft tileInsertionOptions

let moveRight tileInsertionOptions =
    move GameState.shiftRight tileInsertionOptions

let moveUp tileInsertionOptions =
    move GameState.shiftUp tileInsertionOptions

let moveDown tileInsertionOptions =
    move GameState.shiftDown tileInsertionOptions
