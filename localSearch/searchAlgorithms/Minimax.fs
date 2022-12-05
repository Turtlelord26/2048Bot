module LocalSearch.Minimax

open Game
open Random
open SearchTree

let private ninfinity = -1. * infinity

let private initMinV = (infinity, SearchTree.empty)

let private initMaxV = (ninfinity, SearchTree.empty)

let private compareVs comparator (v1, v1tree) (v2, v2tree) =
    if comparator v2 v1
    then (v2, v2tree)
    else (v1, v1tree)

let private minV = compareVs (<)

let private maxV = compareVs (>)

let minimaxSearch scoreTree tileInsertionOptions depth state =

    let rec depthTest depth tree =
        if
            depth <= 1
        then
            (scoreTree tree, tree)
        else
            minValue depth tree
    
    and maxRecurThroughChildren depth v children =
        match Seq.tryHead children with
        | Some child ->
            let newV =
                depthTest depth child
                |> maxV v
            maxRecurThroughChildren depth newV (Seq.tail children)
        | None ->
            v
    
    and maxValue depth =
        SearchTree.expandNodeWithoutInsertion
        >> SearchTree.getChildren
        >> maxRecurThroughChildren depth initMaxV

    and minRecurThroughChildren depth v children =
        match Seq.tryHead children with
        | Some child ->
            let newV =
                maxValue (depth - 1) child
                |> minV v
            minRecurThroughChildren depth newV (Seq.tail children)
        | None ->
            v
    
    and minValue depth =
        tileInsertionOptions
        |> GameState.expandInsertionPossibilities
        |> SearchTree.mapStateToMany
        >> minRecurThroughChildren depth initMinV
    
    let root =
        state
        |> SearchTree.toSearchTreeRoot
    
    let choice =
        root
        |> maxValue depth
        |> snd
        |> SearchTree.getRootCauseAction
    
    match choice with
    | Some _ -> choice
    | None ->
        match
            root
            |> SearchTree.expandNodeWithoutInsertion
            |> SearchTree.getChildren
            |> Seq.map SearchTree.getRootCauseAction
            |> randomElementIfNonempty
        with
        | Some actionOption -> actionOption
        | None -> None

