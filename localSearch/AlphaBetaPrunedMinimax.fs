module LocalSearch.AlphaBetaPrunedMinimax

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

let alphaBetaMinimaxSearch scoreTree tileInsertionOptions depth =

    let rec depthTest depth alpha beta tree =
        if
            depth <= 1
        then
            (scoreTree tree, tree)
        else
            minValue depth alpha beta tree
    
    and maxRecurThroughChildren depth (v, vtree) alpha beta children =
        match Seq.tryHead children with
        | Some child ->
            let (newV, newVTree) =
                depthTest depth alpha beta child
                |> maxV (v, vtree)
            if
                betaTest (newV, newVTree) beta
            then
                (newV, newVTree)
            else
                maxRecurThroughChildren depth (newV, newVTree) (max alpha newV) beta (Seq.tail children)
        | None ->
            (v, vtree)
    
    and maxValue depth alpha beta tree =
        tree |> SearchTree.expandNodeWithoutInsertion
        |> SearchTree.getChildren
        |> maxRecurThroughChildren depth initMaxV alpha beta

    and minRecurThroughChildren depth (v, vTree) alpha beta children =
        match Seq.tryHead children with
        | Some child ->
            let (newV, newVTree) =
                maxValue (depth - 1) alpha beta child
                |> minV (v, vTree)
            if
                alphaTest (newV, newVTree) alpha
            then
                (newV, newVTree)
            else
                minRecurThroughChildren depth (newV, newVTree) alpha (min beta newV) (Seq.tail children)
        | None ->
            (v, vTree)
    
    and minValue depth alpha beta tree =
        tree
        |> (SearchTree.expandInsertionPossibilities tileInsertionOptions |> SearchTree.mapStateToMany)
        |> minRecurThroughChildren depth initMinV alpha beta
    
    SearchTree.toSearchTreeRoot
    >> maxValue depth initAlpha initBeta
    >> snd
    >> SearchTree.getRootCauseAction