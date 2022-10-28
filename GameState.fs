namespace Game

open Random

type ShiftDirection =
    | ByRow
    | ByColumn

type GameState = State of Tile[,] * int
    with

    member this.board =
        let (State (board, _)) = this
        board

    member this.score =
        let (State (_, score)) = this
        score

    member this.maxRow =
        Array2D.length2 this.board - 1

    member this.maxCol =
        Array2D.length1 this.board - 1
    
    static member private applyOverCopy func (state: GameState) =
        func (State (state.board |> Array2D.copy, state.score))
    
    static member private addRandomTile (state: GameState) =

        let newTile = 
            match random.next 2 with
            | 0 -> Exponent 1
            | _ -> Exponent 2
        
        let indexTiles row col tile =
            (tile, (row, col))
        
        let getAllIndexedTiles (board: (Tile * (int * int))[,]) =
            seq {for i in 0..state.maxRow do board[i,*]}
            |> Array.concat
        
        let isEmpty tile =
            match tile with
            | Blank -> true
            | _ -> false
        
        let randomTile tiles =
            let index =
                tiles
                |> Seq.length
                |> random.next
            Seq.item index tiles
        
        let addTile (board: Tile[,]) =

            let (row, col) =
                board
                |> Array2D.mapi indexTiles
                |> getAllIndexedTiles
                |> Seq.filter (fst >> isEmpty)
                |> randomTile
                |> snd

            board[row,col] <- newTile
            board
        
        let applyToBoard func (State (tiles, score)) =
            (func tiles, score)
            |> State
        
        state
        |> GameState.applyOverCopy (applyToBoard addTile)
    
    static member private shiftRow indexing (board: Tile[,]) maxCol row =
        board[row, 0..maxCol]
        |> Tile.shiftTiles indexing
    
    static member private shiftCol indexing (board: Tile[,]) maxRow col =
        board[0..maxRow, col]
        |> Tile.shiftTiles indexing

    static member private shiftAll shiftType indexing (state: GameState) =

        let (State (tiles, score)) = state

        let rangeTo max = [0..max]
        
        let (listLen, numLists) =
            match shiftType with
            | ByRow -> state.maxCol, state.maxRow
            | ByColumn -> state.maxRow, state.maxCol

        let shiftfn = 
            match shiftType with
            | ByRow -> GameState.shiftRow indexing tiles listLen
            | ByColumn -> GameState.shiftCol indexing tiles listLen

        let sumScores (rows, scores) =
            (rows, List.sum scores)

        let shiftAndScore shiftFn =
            rangeTo
            >> List.map shiftFn
            >> List.unzip
            >> sumScores

        let (shiftedCols, shiftScore) = shiftAndScore shiftfn numLists
        
        match shiftType with
        | ByRow ->
            for index in 0..numLists do
                tiles[index, 0..listLen] <- shiftedCols[index]
        | ByColumn ->
            for index in 0..numLists do
                tiles[0..listLen, index] <- shiftedCols[index]
        
        State (tiles, score + shiftScore)
        |> GameState.addRandomTile

    static member shiftLeft (state: GameState) =
        state
        |> GameState.applyOverCopy (GameState.shiftAll ByRow Forward)

    static member shiftRight (state: GameState) =
        state
        |> GameState.applyOverCopy (GameState.shiftAll ByRow Backward)
    
    static member shiftUp (state: GameState) =
        state
        |> GameState.applyOverCopy (GameState.shiftAll ByColumn Forward)
    
    static member shiftDown (state: GameState) =
        state
        |> GameState.applyOverCopy (GameState.shiftAll ByColumn Backward)
