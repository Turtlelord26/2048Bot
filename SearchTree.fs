namespace SearchTree

open Action
open Game

type SearchTree = 
    Tree of 
        state: GameState
        * action: Action option
        * parent: SearchTree option
        * children: SearchTree seq
    with

    static member empty = Tree (GameState.empty, None, None, Seq.empty)

    static member private getState (Tree (state, _, _, _)) = state

    static member mapState mapper (Tree (state, _, _, _)) = mapper state
        
    static member private actionOf (Tree (_, action, _, _)) = action

    static member getChildren (Tree (_, _, _, children)) = children

    static member toSearchTreeRoot state =
        Tree (state, None, None, Seq.empty)
    
    static member mapStateToMany mapper (Tree (state, action, parent, children)) =
        
        let returnToTree newState =
            Tree (newState, action, parent, children)
        
        state
        |> mapper
        |> Seq.map returnToTree
    
    static member private expandLegalMoves parentTree =

        let parentState = parentTree |> SearchTree.getState 

        let toChildTree action parent child =
            Tree (child, Some action, Some parent, Seq.empty)

        seq {GameState.shiftLeft parentState |> toChildTree Left parentTree;
                GameState.shiftRight parentState |> toChildTree Right parentTree;
                GameState.shiftUp parentState |> toChildTree Up parentTree;
                GameState.shiftDown parentState |> toChildTree Down parentTree;}
        |> Seq.filter (SearchTree.mapState ((<>) parentState))
    
    static member private expandNode insertTile tree =
        
        let childrenOf =
            SearchTree.expandLegalMoves
            >> Seq.map insertTile
            >> Seq.concat
        
        let (Tree (state, action, parent, children)) = tree

        match children |> Seq.tryHead with
        | Some _ ->
            tree
        | None ->
            Tree (state, action, parent, childrenOf tree)
    
    static member expandNodeWithoutInsertion =
        SearchTree.expandNode Seq.singleton
    
    static member expandNodeWithExhaustiveInsertion =
        GameState.expandInsertionPossibilities
        >> SearchTree.mapStateToMany
        >> SearchTree.expandNode

    static member private pathToRoot tree =
        
        let rec climb path tree =
            match tree with
            | Tree (_, _, None, _) ->
                path
            | Tree (_, _, Some parent, _) ->
                climb (parent :: path) parent
        
        climb [tree] tree
    
    static member getRootCauseAction =
        SearchTree.pathToRoot
        >> Seq.tryPick SearchTree.actionOf
