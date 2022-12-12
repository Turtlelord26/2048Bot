namespace Game

open TupleUtils

type GameState =
    State of 
        board: Tile[,]
        * score: int
    with

    static member empty = GameState.initialState 0 0
    
    static member boardOf (State (board, _)) = board

    static member scoreOf (State (_, score)) = score
    
    static member initialState rows cols =
        State (Array2D.create rows cols Blank, 0)
    
    static member private applyOverCopy func (State (board, score)) =
        (State (Array2D.copy board, score))
        |> func
    
    static member private assignTile tile row col =

        let assignTileToBoardIndex tile row col (State (board, score)) =
            (board |> Board.writeTileToBoard tile row col, score)
            |> State
        
        assignTileToBoardIndex tile row col
        |> GameState.applyOverCopy
    
    static member addRandomTile options =
        
        let addRandomTileToBoard (State (board, score)) =
            (board |> Board.addRandomTileToRandomBlank options, score)
            |> State

        addRandomTileToBoard
        |> GameState.applyOverCopy
    
    static member private shiftRows indexing =
        Board.getRows
        >> List.map (Board.shiftTiles indexing)
    
    static member private shiftCols indexing =
        Board.getCols
        >> List.map (Board.shiftTiles indexing)

    static member private shiftAll shiftType indexing (State (board, oldScore)) =
        
        let writeRows (board: Tile[,]) tiles =
            List.mapi (fun row shiftedRow -> board[row, *] <- shiftedRow) tiles |> ignore
            board
        
        let writeCols (board: Tile[,]) tiles =
            List.mapi (fun col shiftedCol -> board[*, col] <- shiftedCol) tiles |> ignore
            board
        
        let shiftBoard, writeBoard = 
            match shiftType with
            | ByRow -> GameState.shiftRows indexing, writeRows board
            | ByColumn -> GameState.shiftCols indexing, writeCols board

        let shiftAndScore =
            shiftBoard
            >> List.unzip
            >> mapSnd List.sum

        board
        |> shiftAndScore 
        |> mapFst writeBoard
        |> mapSnd ((+) oldScore)
        |> State

    static member shiftLeft =
        GameState.shiftAll ByRow Forward
        |> GameState.applyOverCopy

    static member shiftRight =
        GameState.shiftAll ByRow Backward
        |> GameState.applyOverCopy
    
    static member shiftUp =
        GameState.shiftAll ByColumn Forward
        |> GameState.applyOverCopy
    
    static member shiftDown =
        GameState.shiftAll ByColumn Backward
        |> GameState.applyOverCopy
    
    static member expandInsertionPossibilities possibleTiles state =
    
        let addTileAtIndex state (row, col) tile =
            seq {state |> GameState.assignTile tile row col; }

        state
        |> GameState.boardOf
        |> Board.getBlankTileIndices
        |> Seq.map (addTileAtIndex state)
        |> Seq.allPairs possibleTiles
        |> Seq.collect (fun (tile, assignment) -> tile |> assignment)
