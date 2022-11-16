namespace ValidatedGameState

open Game

type Status =
    | Valid
    | Victory
    | Defeat
    with

    static member toString status =
        match status with
        | Victory -> "Victory"
        | Defeat -> "Defeat"
        | Valid -> "Game Not Terminated"

type ValidatedGameState =
    VState of
        state: GameState
        * status: Status
    with
        
    static member stateOf (VState (state, _)) = state

    static member statusOf (VState (_, status)) = status

    static member private isStatusVictory =
        GameState.boardOf
        >> Board.getTiles
        >> Seq.contains (Exponent 11)

    static member private isStatusValid state =

        let board = GameState.boardOf state
        
        let combinableTiles (tile1, tile2) =
            match tile1, tile2 with
            | Exponent a, Exponent b ->
                a = b
            | _ ->
                false

        let rowsAndCols board =
            Board.getCols board
            |> Seq.append (Board.getRows board)
        
        let containsCombinableTiles =
            rowsAndCols
            >> Seq.collect (Seq.pairwise)
            >> Seq.exists combinableTiles
        
        let containsBlanks =
            Board.getTiles
            >> Seq.exists Tile.isBlank

        containsBlanks board
        || containsCombinableTiles board


    /// Detects game status. A given state may be Valid, Victory, or Defeat.
    /// A Victory state contains a tile of 2048 (equiv. Exponent 11).
    /// A Defeat state is a state that contains no Blank tiles and no adjacent pair of equal-valued tiles.
    /// Defeat detection short-circuits to false if a Blank tile exists, since the other check requires significantly more computation.
    /// A Valid state is any other state.
    static member private evaluate state =
        if state |> ValidatedGameState.isStatusVictory
        then
            Victory
        else if state |> ValidatedGameState.isStatusValid
        then
            Valid
        else
            Defeat
    
    static member wrap state =
        VState (state, state |> ValidatedGameState.evaluate)
    
    static member map mapper (VState (state, _)) =
        let newState = mapper state
        VState (newState, newState |> ValidatedGameState.evaluate)
