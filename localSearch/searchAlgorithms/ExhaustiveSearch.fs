module LocalSearch.ExhaustiveSearch

open SearchTree

let exhaustiveSearch evaluationFunction tileInsertionOptions depth tree =

    let rec searchToDepth depths =
        match depths with
        | i when i > 1 ->
            SearchTree.expandNodeWithExhaustiveInsertion tileInsertionOptions
            >> SearchTree.getChildren
            |> Seq.collect
            >> searchToDepth (depths - 1)
        | 1 ->
            SearchTree.expandNodeWithoutInsertion
            >> SearchTree.getChildren
            |> Seq.collect 
        | 0 ->
            id
        | _ ->
            fun _ -> Seq.empty
    
    let immediateChildren =
        tree
        |> SearchTree.expandNodeWithExhaustiveInsertion tileInsertionOptions
        |> SearchTree.getChildren
    
    let depthChildren =
        immediateChildren
        |> searchToDepth (depth - 1)
    
    let bestChild =
        depthChildren
        |> evaluationFunction
    
    bestChild, immediateChildren
