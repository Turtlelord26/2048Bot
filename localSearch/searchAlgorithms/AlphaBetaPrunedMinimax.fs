module LocalSearch.AlphaBetaPrunedMinimax

open Game
open Random
open SearchTree

let private ninfinity = -1. * infinity

let private initAlpha = ninfinity

let private initBeta = infinity

let private initMinV = (infinity, SearchTree.empty)

let private initMaxV = (ninfinity, SearchTree.empty)

let private compareVs comparator (v1, v1tree) (v2, v2tree) =
    if comparator v2 v1
    then (v2, v2tree)
    else (v1, v1tree)

let private minV = compareVs (<)

let private maxV = compareVs (>)

let private compareVToConst comparator (v, _) value =
    comparator v value

let private alphaTest v alpha =
    compareVToConst (<=) v alpha

let private betaTest v beta =
    compareVToConst (>=) v beta

let alphaBetaMinimaxSearch scoreTree tileInsertionOptions depth state =

    let rec depthTest depth alpha beta tree =
        if
            depth <= 1
        then
            (scoreTree tree, tree)
        else
            minValue depth alpha beta tree
    
    and maxRecurThroughChildren depth v alpha beta children =
        match Seq.tryHead children with
        | Some child ->
            let newV =
                depthTest depth alpha beta child
                |> maxV v
            if
                betaTest newV beta
            then
                newV
            else
                maxRecurThroughChildren depth newV (fst newV |> max alpha) beta (Seq.tail children)
        | None ->
            v
    
    and maxValue depth alpha beta =
        SearchTree.expandNodeWithoutInsertion
        >> SearchTree.getChildren
        >> maxRecurThroughChildren depth initMaxV alpha beta

    and minRecurThroughChildren depth v alpha beta children =
        match Seq.tryHead children with
        | Some child ->
            let newV =
                maxValue (depth - 1) alpha beta child
                |> minV v
            if
                alphaTest newV alpha
            then
                newV
            else
                minRecurThroughChildren depth newV alpha (fst newV |> min beta) (Seq.tail children)
        | None ->
            v
    
    and minValue depth alpha beta =
        tileInsertionOptions
        |> GameState.expandInsertionPossibilities
        |> SearchTree.mapStateToMany
        >> minRecurThroughChildren depth initMinV alpha beta
    
    
    
    let root =
        state
        |> SearchTree.toSearchTreeRoot
    
    let choice =
        root
        |> maxValue depth initAlpha initBeta
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
