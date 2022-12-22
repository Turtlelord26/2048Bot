### 2048 Bot

Course project at Seattle University, Fall Quarter 2022, CPSC5610 Artificial Intelligence

Creating an agent to play the game 2048

This project is written in F#, and seeks to adhere as closely as practicable to idiomatic functional programming.
Mutable data structures appear briefly in low-level game state functions, but are cloned from their source before purely local mutation and then returned, as if immutable.
The bidirectional slicing capability of Array2D was just too convenient not to use as the board state representation.

main in Program.fs is the entry point.

The game engine is defined in the gameplay subdirectory.
- Tile is a sum type that represents a single space on the 2048 board, either containing a value or being blank.
- Board contains low-level functions that shift lists of Tiles and apply other state changes to the underlying Tile Array2D.
- GameState is a data structure containing a 2DArray of Tiles and a score, and contains higher-level shift logic to effect moves to the Left, Right, Up, or Down, along with other state-modifying functions.
- Moves combines GameState shifts with a random Tile insertion.
- ValidatedGameState further augments state information by wrapping a GameState with a status value that indicates whether the game is in a victory or loss state.

The tests subdirectory contains a number of shift tests that compare an expected state with the result of shifting an initial state.

#### Milestone 4: 
Initialize a starting board with two random 2 tiles, then play the game with minimax until victory (making a 2048 tile) or defeat (having no legal moves).
The game allows the four moves Left, Right, Up, and Down whether or not they move any tiles, then places a 2 or a 4 tile in a random location with equal probability of all possible insertions.
Uses game score as the minimax utility.

I use a recursively-defined search tree (in SearchTree.fs) to expand from a given state and to collect lookaheads.

Local search algorithms and utilities are defined in the localSearch directory.
The program currently supports 4 search strategies via command line arguments.
- `MinimaxMaximumScore`: Standard minimax algorithm using game score as utility.
- `ABPrunedMaximumScore`: Alpha-beta pruned minimax, otherwise same as above.
- `ExpectimaxMaximumScore`: Expectimax algorithm using game score as utility.
- `ExpectimaxEUMR`: Expectimax algorithm using a multistage heuristic to assign utility to states. The EUMR* scores based on quantity of blank tiles (emptiness), then uniformity of numbered tiles, then monotonicity of numbered tiles, using subsequent evaluators only to break ties in earlier ones. If all three tie, a random remaining action is chosen.

Usage: on command line in the project directory, `dotnet run {keyword} {lookahead depth} {trials}`

Using the user-specified lookahead depth (integer >= 2), the program runs the user-specified number of trials (integer >= 1), and outputs the result of the highest-scoring game to console. In order, it outputs the number of moves taken, the sequence of moves, the final arrangement of the board (human-readable), and the score.
The program also prints the runtime of all trials.

`dotnet run test` runs unit tests on the game engine.

*The EUMR heuristic is based on the heuristic by the same name as described in _Composition of Basic Heuristics for the Game 2048_, by Kohler, Migler, and Khosmood.
