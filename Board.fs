module Board

open Array2DUtils
open Game
open Operators
open Random
open TupleUtils

let getRows board =

    let getRow (board: Tile[,]) index =
        board[index, *]

    board
    |> rowIndices
    |> List.map (getRow board)

let getCols board =

    let getCol (board: Tile[,]) index =
        board[*, index]

    board
    |> colIndices
    |> List.map (getCol board)

let getIndexedBlankTiles =

    let indexTiles row col tile =
        (tile, (row, col))
    
    let sequenceIndexedTiles board =
        seq {for i in rowIndices board do board[i,*]}
        |> Seq.concat
    
    Array2D.mapi indexTiles
    >> sequenceIndexedTiles
    >> Seq.filter (fst >> Tile.isBlank)

let writeTileToBoard tile row col (tiles: Tile[,]) =
        tiles[row, col] <- tile
        tiles

let addRandomTileToRandomBlank options =

    let indicesOfRandomBlank =
        getIndexedBlankTiles
        >> Seq.map snd
        >> randomElementIfNonempty
    
    let addTileToRandomBlankIfAny tile board =
        match board |> indicesOfRandomBlank with
        | Some (row, col) ->
            writeTileToBoard tile row col board
        | None ->
            board
    
    options
    |> randomElement
    |> addTileToRandomBlankIfAny

let private shift tileList =

    let combineExponents = (+) 1

    let merge firstTile secondTile =
        match firstTile, secondTile with
        | Exponent firstValue, Exponent secondValue
            when firstValue = secondValue ->
            firstValue |> combineExponents |> Exponent, Blank
        | Exponent _, _ ->
            firstTile, secondTile
        | Blank, _ ->
            Blank, secondTile
    
    let sortTiles score unprocessedTail blanks processedValues mergedPair =
        match mergedPair with
        Exponent firstVal, Exponent secondVal ->
            score,
            (Exponent secondVal :: unprocessedTail),
            blanks,
            (Exponent firstVal :: processedValues)
        | Exponent value, Blank
        | Blank, Exponent value->
            (Exponent value |> Tile.score |> (+) score),
            unprocessedTail,
            (Blank :: blanks),
            (Exponent value :: processedValues)
        | Blank, Blank ->
            score,
            unprocessedTail,
            (Blank :: Blank :: blanks),
            processedValues

    let rec shiftTiles score unprocessedTiles blanks processedValues  =
        match unprocessedTiles with
        //Take two tiles at a time, attempt merge, and place results in appropriate lists
        | first :: second :: tail ->
            merge first second
            |> sortTiles score tail blanks processedValues
            ||||> shiftTiles
        //Penultimate condition, only one tile left to look at, no merge possible
        | first :: tail ->
            shiftTiles score tail blanks (first :: processedValues)
        //Terminal condition, no more tiles to merge
        | [] ->
            List.append blanks processedValues, score

    let blanks =
        tileList
        |> List.filter (Tile.isBlank)

    let values =
        tileList
        |> List.filter (not << Tile.isBlank)
    
    shiftTiles 0 values blanks []

let shiftTiles indexing =

    let preOrdering, postOrdering =
        match indexing with
        | Forward -> id, List.rev
        | Backward -> List.rev, id

    List.ofArray
    >> preOrdering
    >> shift
    >> mapFst (postOrdering >> Array.ofList)
