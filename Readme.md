### 2048 Bot

Course project at Seattle University, Fall Quarter 2022, CPSC5610 Artificial Intelligence

Creating an agent to play the game 2048

This project is written in F#, and seeks to adhere as closely as practicable to idiomatic functional programming.
Mutable data structures appear briefly in low-level game state functions, but are cloned from their source before purely local mutation and then returned, as if immutable.
The bidirectional slicing capability of Array2D was just too convenient not to use as the board state representation.

main in Program.fs is the entry point.

The game engine is defined between the GameState and Tile files. Tile represents a single space on the 2048 board, while GameState is a 2DArray of Tiles and a score.
Tile contains functions to shift a one-dimensional list of tiles, while GameState contains the logic to invoke this shift in different ways to effect Left, Right, Up, or Down moves.

The tests subdirectory contains a number of shift tests that compare an expected state with the result of shifting an initial state.

#### Milestone 1: 
From an arbitrary board, use 3-move lookahead via brute-force search to determine the highest-scoring course of action. Deterministic addition of additional Twos.

I use a recursively-defined search tree (in Search.fs) to expand from the initial state and collect all of the three-moves-ahead states.

Usage: on command line in the project directory, do `dotnet run Program.fs 2048_in.txt 2048_out.txt
