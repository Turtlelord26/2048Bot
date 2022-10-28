namespace Game

open Operators
open TupleUtils

type Indexing =
    | Forward
    | Backward

type Tile =
    | Exponent of int
    | Blank
    with

    static member isBlank tile =
        match tile with
        | Blank -> true
        | Exponent _ -> false
    
    static member scoreValue =
        float
        >> ( ** ) 2.
        >> int
    
    static member score tile =
        match tile with
        | Exponent value -> value |> Tile.scoreValue
        | Blank -> 0
    
    static member private shift tileList =

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
                (value |> Tile.scoreValue |> (+) score),
                unprocessedTail,
                (Blank :: blanks),
                (Exponent value :: processedValues)
            | Blank, Blank ->
                score,
                unprocessedTail,
                (Blank :: Blank :: blanks),
                processedValues

        let rec shiftTiles score unprocessedTiles blanks  processedValues  =
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
    
    static member shiftTiles indexing =

        let preOrdering, postOrdering =
            match indexing with
            | Forward -> id, List.rev
            | Backward -> List.rev, id

        List.ofArray
        >> preOrdering
        >> Tile.shift
        >> mapFst (postOrdering >> Array.ofList)
