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
- `ExpectimaxEUMR`: Expectimax algorithm using a multistage heuristic to assign utility to states. The EUMR scores based on quantity of blank tiles (emptiness), then uniformity of numbered tiles, then monotonicity of numbered tiles, using subsequent evaluators only to brea ties in earlier ones. If all three tie, a random remaining action is chosen.

Usage: on command line in the project directory, `dotnet run {keyword} {lookahead depth} {trials}`

Using the user-specified lookahead depth (integer >= 2), the program runs the user-specified number of trials (integer >= 1), and outputs the result of the highest-scoring game to console. In order, it outputs the number of moves taken, the sequence of moves, the final arrangement of the board (human-readable), and the score.
The program also prints the runtime of all trials.

`dotnet run test` runs unit tests.

Notes to grader (from milestone 4):
- This single codebase completes base milestone 4 requirements and all three bonuses. `MinimaxMaximumScore` is the search specific in milestone 4 proper. 
- For bonus 1, the depth can be varied in the commmand line arguments as described above, and the runtime printout provides performance information for different values. 
- For bonus 2, the `ABPrunedMaximumScore` argument runs the specified alpha-beta pruned version of minimax. 
- For bonus 3, I use the EUMR heuristic as described in _Composition of Basic Heuristics for the Game 2048_, by Kohler, Migler, and Khosmood in the search run by `ExpectimaxEUMR`. It evaluates on Emptiness, Uniformity, and then Monotonicity, using subsequent evaluators only to break ties from earlier ones, and picks randomly if all three tie. In the authors' testing, this heuristic composition achieved the highest median and maximum score. Since this is an expectimax search, it is also averaging the utility of lookahead states by the (first) action that led to each one.

Notes to grader (from milestone 2):
- I have no explicit "current and next score" in my code. For maximizing local search, the maximum "current and next score" as defined in the assignment will always be the same state that just has the maximum total score, so I use that instead. For random local search, I save the current state's score in a function closure to compare the lookahead states' scores against.
- Milestone 2 describes a search where, from our starting state, we make a move, insert a tile, and make a second move, accounting for all possibilities in each of the three steps. I do this in the function LocalSearch.nthChildren, which parametrizes the depth of lookaheads. For each depth, it makes all four moves and makes all possible insertions, except on the final depth where it does not make insertions. For depth 2, this is the same as specified in milestone 2, and all current uses of this function supply 2 as the depth argument. I did some testing at depth 3, but the program started taking a long time to produce output that often wasn't much better.
- I am aware that search methods 3-5 are extraneous to milestone requirements.
