module LocalSearch.ExhaustiveSearch

open SearchTree

let exhaustiveSearch evaluationFunction tileInsertionOptions depth =

    let rec searchToDepth depths =
        match depths with
        | i when i > 1 ->
            Seq.map (SearchTree.expandNodeWithExhaustiveInsertion tileInsertionOptions)
            >> Seq.collect SearchTree.getChildren
            >> searchToDepth (depths - 1)
        | 1 ->
            Seq.map SearchTree.expandNodeWithoutInsertion
            >> Seq.collect SearchTree.getChildren
        | 0 ->
            id
        | _ ->
            fun _ -> Seq.empty

    SearchTree.toSearchTreeRoot
    >> Seq.singleton
    >> searchToDepth depth
    >> evaluationFunction
