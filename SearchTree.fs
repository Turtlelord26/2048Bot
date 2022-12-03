module SearchTree

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
        * children: SearchTree seq
    with

    static member empty = Empty

    static member mapState mapper ifEmpty searchTree =
        match searchTree with
        | Tree (state, _, _, _) ->
            mapper state
        | Empty ->
            ifEmpty
        
    static member actionOf tree =
        match tree with
        | Tree (_, action, _, _) ->
            action
        | Empty ->
            None

    static member toSearchTreeRoot state =
        Tree (state, None, Empty, Seq.empty)
    
    static member mapStateToMany mapper tree =
        match tree with
        | Tree (state, action, parent, children) ->
            let returnToTrees =
                Seq.map (fun newState -> Tree (newState, action, parent, children))
            state
            |> mapper
            |> returnToTrees
        | Empty ->
            Seq.empty
    
    static member private expandLegalMoves parentTree parentState =

        let toChildTree action parent child =
            Tree (child, Some action, parent, Seq.empty)

        seq {GameState.shiftLeft parentState |> toChildTree Left parentTree;
                GameState.shiftRight parentState |> toChildTree Right parentTree;
                GameState.shiftUp parentState |> toChildTree Up parentTree;
                GameState.shiftDown parentState |> toChildTree Down parentTree;}
        |> Seq.filter (SearchTree.mapState ((<>) parentState) false)
    
    static member private expandNode insertTile tree =
        
        let childrenOf parentTree =
            SearchTree.expandLegalMoves parentTree
            >> Seq.map insertTile
            >> Seq.concat

        match tree with
        | (Tree (state = state; action = action; parent = parent; children = _;)) ->
            Tree (state,
                  action,
                  parent,
                  childrenOf tree state)
        | Empty ->
            Empty
    
    static member expandNodeWithoutInsertion =
        SearchTree.expandNode Seq.singleton
    
    static member expandInsertionPossibilities possibleTiles state =
        //If this stays a separate function move it over to GameState.
        let addTileAtIndex state (row, col) tile =
            seq {state |> GameState.assignTile tile row col; }

        state
        |> GameState.boardOf
        |> Board.getBlankTileIndices
        |> Seq.map (addTileAtIndex state)
        |> Seq.allPairs possibleTiles
        |> Seq.map (fun (tile, assignment) -> tile |> assignment)
        |> Seq.concat
    
    static member expandNodeWithExhaustiveInsertion =
        SearchTree.expandInsertionPossibilities
        >> SearchTree.mapStateToMany
        >> SearchTree.expandNode

    static member getChildren tree =
        match tree with
        | Tree (_, _, _, children) ->
            children
        | Empty ->
            Seq.empty

    static member pathToRoot tree =
        
        let rec climb path tree =
            match tree with
            | Tree (_, None, _, _)
            | Empty ->
                path
            | Tree (_, _, parent, _) ->
                climb (parent :: path) parent
        
        climb [tree] tree
    
    static member rootOf tree =

        let rec parentOf tree =
            match tree with
            | Tree (_, None, _, _)
            | Empty ->
                tree
            | Tree (_, _, parent, _) ->
                parentOf parent
        
        parentOf tree
    
    static member getRootCauseAction =
        SearchTree.pathToRoot
        >> Seq.tryPick SearchTree.actionOf
