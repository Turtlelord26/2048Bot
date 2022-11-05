### 2048 Bot

Course project at Seattle University, Fall Quarter 2022, CPSC5610 Artificial Intelligence

Creating an agent to play the game 2048

This project is written in F#, and seeks to adhere as closely as practicable to idiomatic functional programming.
Mutable data structures appear briefly in low-level game state functions, but are cloned from their source before purely local mutation and then returned, as if immutable.
The bidirectional slicing capability of Array2D was just too convenient not to use as the board state representation.

main in Program.fs is the entry point.

The game engine is defined between the GameState and Tile files. Tile represents a single space on the 2048 board, while GameState is a 2DArray of Tiles and a score.
Tile contains functions to shift a one-dimensional list of tiles, while GameState contains the logic to invoke this shift in different ways to effect Left, Right, Up, or Down moves. ValidatedGameState further augments state information by wrapping a GameState with a status value that indicates whether the game is in a victory or loss state.

The tests subdirectory contains a number of shift tests that compare an expected state with the result of shifting an initial state.

#### Milestone 2: 
Initialize a starting board with two random 2 tiles, then play the game with local search until victory (making a 2048 tile) or defeat (having no legal moves).
The game allows the four moves Left, Right, Up, and Down whether or not they move any tiles, then places a 2 or a 4 tile in a random location with equal probability of all possible insertions.
Use both random-better-state local search and best-state local search with 2-move lookahead, where better states have higher scores.

I use a recursively-defined search tree (in SearchTree.fs) to expand from a given state and to collect lookaheads.

Local search algorithms are defined in LocalSearch.fs.
I have implemented 5 different forms of local search, each of which is invoked with a keyword argument when running the program.
- randomLocalSearch: Selects a random lookahead state to determine its next move.
- maximalScoreLocalSearch: Selects the lookahead state with highest score. Breaks ties randomly.
- maximalBlanksLocalSearch: Selects the lookahead state with the highest number of blank tiles. Breaks ties randomly.
- maximalBlanksThenScoreLocalSearch: Selects the lookahead state with the highest number of blank tiles. Breaks ties by score, then randomly.
- maximalExpectedScoreLocalSearch: Selects the lookahead state with the highest expected score, averaging all possibilities that result from each of the four moves. Breaks ties arbitrarily.

I was surprised to find that methods 3 and 4 offer apparently no significant performance improvement over method 2.
Method 5 does offer some improvement, but my program still has yet to win a game. Current high score is from method 5 at 16204.
There are clear problems in the bot's handling of high-value tiles that I hope we are targeting in future milestones.

Usage: on command line in the project directory, `dotnet run {keyword}`

Output:
- For no arguments, will print usage
- For argument `test`, will run unit tests
- For the other arguments enumerated above, the program will print the results of the best run of the game to the console. In order, it outputs the number of moves taken, the sequence of moves, the final arrangement of the board (human-readable), the score, and the status (Defeat or Victory).

Notes to grader:
- I have no explicit "current and next score" in my code. For maximizing local search, the maximum "current and next score" as defined in the assignment will always be the same state that just has the maximum total score, so I use that instead. For random local search, I save the current state's score in a function closure to compare the lookahead states' scores against.
- Milestone 2 describes a search where, from our starting state, we make a move, insert a tile, and make a second move, accounting for all possibilities in each of the three steps. I do this in the function LocalSearch.nthChildren, which parametrizes the depth of lookaheads. For each depth, it makes all four moves and makes all possible insertions, except on the final depth where it does not make insertions. For depth 2, this is the same as specified in milestone 2, and all current uses of this function supply 2 as the depth argument. I did some testing at depth 3, but the program started taking a long time to produce output that often wasn't much better.
