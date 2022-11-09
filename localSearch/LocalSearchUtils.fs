module LocalSearchUtils

open Game
open LocalSearchActions
open Search

let private playWithSearch searchAndExecuteNextAction onVictory onDefeat =

    let rec randomLocalSearchIteration actionsTaken vstate =
        match vstate |> ValidatedGameState.statusOf with
        | Valid ->
            searchAndExecuteNextAction randomLocalSearchIteration vstate actionsTaken
        | Victory ->
            onVictory vstate actionsTaken
        | Defeat ->
            onDefeat vstate actionsTaken
    
    randomLocalSearchIteration []

let bestTrialOfPlayWithSearch searchMethod returnFromTerminalState trials initialState =

    let iterateLocalSearch = searchAndExecuteNextAction searchMethod

    let localSearch = playWithSearch iterateLocalSearch returnFromTerminalState returnFromTerminalState

    let score =
        fst
        >> ValidatedGameState.stateOf
        >> GameState.scoreOf

    seq {for _ in 1..trials do localSearch initialState}
    |> Seq.maxBy score

let nthChildren depth =

    let rec searchToDepth depths =
        match depths with
        | i when i > 1 ->
            Seq.map SearchTree.expandNodeWithExhaustiveInsertion
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
