module LocalSearch

open Game
open Moves
open Random
open Search

let private chooseNthChildMatchingCondition selectChild depth condition =

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
    >> Seq.replicate 1
    >> searchToDepth depth
    >> Seq.filter condition
    >> selectChild

let private randomNthChildMatchingCondition =
    randomElementIfNonempty
    |> chooseNthChildMatchingCondition

let private bestScoringNthChildMatchingCondition =

    let chooseBestScore trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> Seq.maxBy SearchTree.scoreOf
            |> Some

    chooseBestScore
    |> chooseNthChildMatchingCondition

let private mostOpenSpaceNthChildMatchingCondition =

    let chooseMostOpenSpaces trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> Seq.maxBy SearchTree.countBlanks
            |> Some
    
    chooseMostOpenSpaces
    |> chooseNthChildMatchingCondition

let private mostOpenSpaceWithHighestScoreNthChildMatchingCondition =

    let chooseMostOpenSpacesWithHighestScore trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> Seq.groupBy SearchTree.countBlanks
            |> Seq.maxBy fst
            |> snd
            |> Seq.maxBy SearchTree.scoreOf
            |> Some
    
    chooseMostOpenSpacesWithHighestScore
    |> chooseNthChildMatchingCondition

let private playWithLocalSearch localSearch lookaheadDepth vstate =

    let localSearchLookahead lookaheadDepth state =

        let isScoreIncreased node =
            SearchTree.scoreOf node > GameState.scoreOf state
        
        state
        |> localSearch lookaheadDepth isScoreIncreased
    
    let getNextAction =
        Seq.item 1
        >> SearchTree.actionOf
    
    let determineAction lookaheadDepth =
        localSearchLookahead lookaheadDepth
        >> Option.map SearchTree.pathToRoot
        >> Option.bind getNextAction

    let rec randomLocalSearchIteration lookaheadDepth actionsTaken (vstate: ValidatedGameState) =
        match vstate |> ValidatedGameState.statusOf with
        | Valid ->
            match 
                vstate
                |> ValidatedGameState.stateOf
                |> determineAction lookaheadDepth
            with
            | Some Left ->
                vstate
                |> ValidatedGameState.map moveLeft
                |> randomLocalSearchIteration lookaheadDepth (Left :: actionsTaken)
            | Some Right ->
                vstate
                |> ValidatedGameState.map moveRight
                |> randomLocalSearchIteration lookaheadDepth (Right :: actionsTaken)
            | Some Up ->
                vstate
                |> ValidatedGameState.map moveUp
                |> randomLocalSearchIteration lookaheadDepth (Up :: actionsTaken)
            | Some Down ->
                vstate
                |> ValidatedGameState.map moveDown
                |> randomLocalSearchIteration lookaheadDepth (Down :: actionsTaken)
            | None ->
                vstate, actionsTaken |> Seq.rev
        | Victory
        | Defeat ->
            vstate, actionsTaken |> Seq.rev
    
    vstate
    |> randomLocalSearchIteration lookaheadDepth []

let private bestTrialOfPlayWithLocalSearch localSearch trials lookaheadDepth initialState =

    let score =
        fst
        >> ValidatedGameState.stateOf
        >> GameState.scoreOf

    seq {for _ in 1..trials do yield playWithLocalSearch localSearch lookaheadDepth initialState}
    |> Seq.maxBy score

let playTrialsWithRandomLocalSearch =
    bestTrialOfPlayWithLocalSearch randomNthChildMatchingCondition

let playTrialsWithMaximalLocalSearch =
    bestTrialOfPlayWithLocalSearch bestScoringNthChildMatchingCondition

let playTrialsWithMaximalBlanksLocalSearch =
    bestTrialOfPlayWithLocalSearch mostOpenSpaceNthChildMatchingCondition

let playTrialsWithMaximalBlanksThenScoreLocalSearch =
    bestTrialOfPlayWithLocalSearch mostOpenSpaceWithHighestScoreNthChildMatchingCondition