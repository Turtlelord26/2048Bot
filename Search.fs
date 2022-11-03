module Search

open Board
open Game
open StringUtils

type Action =
    | Left
    | Right
    | Up
    | Down
    with

    static member toString action =
        match action with
        | Left -> "L"
        | Right -> "R"
        | Up -> "U"
        | Down -> "D"

    static member manyToString =
        Seq.map Action.toString
        >> Seq.reduce concatWithComma

type SearchTree =
    | Empty
    | Tree of 
        state: GameState
        * action: Action option
        * parent: SearchTree
        * leftChildren: SearchTree seq
        * rightChildren: SearchTree seq
        * upChildren: SearchTree seq
        * downChildren: SearchTree seq
    with

    static member scoreOf searchTree =
        match searchTree with
        | Tree (state, _, _, _, _, _, _) ->
            GameState.scoreOf state
        | Empty ->
            0

    static member actionOf tree =
        match tree with
        | Tree (_, action, _, _, _, _, _) ->
            action
        | Empty ->
            None

    static member toSearchTreeRoot state =
        Tree (state, None, Empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty)
    
    static member private mapStateToMany mapper tree =
        match tree with
        | Tree (state, action, parent, leftChildren, rightChildren, upChildren, downChildren) ->
            let returnToTrees =
                Seq.map (fun newState -> Tree (newState, action, parent, leftChildren, rightChildren, upChildren, downChildren))
            state
            |> mapper
            |> returnToTrees
        | Empty ->
            Seq.empty
    
    static member private expandNode additionalProcessing tree =
        
        let shift action parent =
            match parent, action with
            | Tree (state, _, _, _, _, _, _), Left ->
                Tree (state |> GameState.shiftLeft, Some Left, parent, Seq.empty,  Seq.empty,  Seq.empty,  Seq.empty)
            | Tree (state, _, _, _, _, _, _), Right ->
                Tree (state |> GameState.shiftRight, Some Right, parent,  Seq.empty,  Seq.empty,  Seq.empty,  Seq.empty)
            | Tree (state, _, _, _, _, _, _), Up ->
                Tree (state |> GameState.shiftUp, Some Up, parent,  Seq.empty,  Seq.empty,  Seq.empty,  Seq.empty)
            | Tree (state, _, _, _, _, _, _), Down ->
                Tree (state |> GameState.shiftDown, Some Down, parent,  Seq.empty,  Seq.empty,  Seq.empty,  Seq.empty)
            | _, _ ->
                Empty
        
        let shiftExpand action =
            shift action
            >> additionalProcessing

        match tree with
        | (Tree (state, action, parent, _, _, _, _)) ->
            Tree (
                state,
                action,
                parent,
                tree |> shiftExpand Left,
                tree |> shiftExpand Right,
                tree |> shiftExpand Up,
                tree |> shiftExpand Down
                )
        | Empty ->
            Empty
    
    static member expandNodeWithoutInsertion =
        SearchTree.expandNode (fun x -> seq {x})
    
    static member expandNodeWithExhaustiveInsertion =

        let addTileAtIndex state (row, col) tile =
            seq {state |> GameState.assignTile tile row col; }
        
        let possibleTiles =
            seq {Exponent 1; Exponent 2}
        
        let expandInsertionPossibilities state =
            state
            |> GameState.boardOf
            |> getIndexedBlankTiles
            |> Seq.map snd
            |> Seq.map (addTileAtIndex state)
            |> Seq.allPairs possibleTiles
            |> Seq.map (fun (tile, assignment) -> tile |> assignment)
            |> Seq.concat

        SearchTree.expandNode (SearchTree.mapStateToMany expandInsertionPossibilities)


    static member getChildren tree =
        match tree with
        | Tree (_, _, _, left, right, up, down) ->
            seq {left; right; up; down}
            |> Seq.concat
        | Empty ->
            Seq.empty

    static member pathToRoot tree =
        
        let rec climb path tree =
            match tree with
            | Tree (_, None, _, _, _, _, _)
            | Empty ->
                path
            | Tree (_, _, parent, _, _, _, _) ->
                climb (parent :: path) parent
        
        climb [tree] tree
