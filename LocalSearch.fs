module LocalSearch

open Average
open Game
open Moves
open Operators
open Random
open Search

let private nthChildrenMatchingCondition depth condition =

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
    >> Seq.filter condition

let private randomNthChildMatchingCondition depth condition =
    nthChildrenMatchingCondition depth condition
    >> randomElementIfNonempty
    >>= SearchTree.getRootCauseAction

let private bestScoringNthChildMatchingCondition depth condition =

    let chooseBestScore trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> Seq.maxBy SearchTree.scoreOf
            |> SearchTree.getRootCauseAction

    nthChildrenMatchingCondition depth condition
    >> chooseBestScore

let private mostOpenSpaceNthChildMatchingCondition depth condition =

    let chooseMostOpenSpaces trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> Seq.maxBy SearchTree.countBlanks
            |> SearchTree.getRootCauseAction
    
    nthChildrenMatchingCondition depth condition
    >> chooseMostOpenSpaces

let private mostOpenSpaceWithHighestScoreNthChildMatchingCondition depth condition =

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
            |> SearchTree.getRootCauseAction
    
    nthChildrenMatchingCondition depth condition
    >> chooseMostOpenSpacesWithHighestScore

let private bestExpectedScoringNthChildMatchingCondition depth condition =

    let groupTreesByRootCauseAction =
        Seq.groupBy SearchTree.getRootCauseAction
    
    let expectedScoreOf =
        Seq.map SearchTree.scoreOf
        >> Seq.map RunningAverage.construct
        >> Seq.sum
        >> RunningAverage.consume
    
    let bestActionByScoreExpectation =
        groupTreesByRootCauseAction
        >> Seq.maxBy (snd >> expectedScoreOf)
        >> fst

    let chooseBestScoreExpectation trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> bestActionByScoreExpectation
    
    nthChildrenMatchingCondition depth condition
    >> chooseBestScoreExpectation

let private playWithLocalSearch localSearch lookaheadDepth vstate =

    let localSearchLookahead lookaheadDepth state =

        let startingScore = GameState.scoreOf state

        let isScoreIncreased node =
            SearchTree.scoreOf node > startingScore
        
        state
        |> localSearch lookaheadDepth isScoreIncreased
    
    let determineAction lookaheadDepth =
        localSearchLookahead lookaheadDepth

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

    seq {for _ in 1..trials do playWithLocalSearch localSearch lookaheadDepth initialState}
    |> Seq.maxBy score

let playTrialsWithRandomLocalSearch =
    bestTrialOfPlayWithLocalSearch randomNthChildMatchingCondition

let playTrialsWithMaximalLocalSearch =
    bestTrialOfPlayWithLocalSearch bestScoringNthChildMatchingCondition

let playTrialsWithMaximalBlanksLocalSearch =
    bestTrialOfPlayWithLocalSearch mostOpenSpaceNthChildMatchingCondition

let playTrialsWithMaximalBlanksThenScoreLocalSearch =
    bestTrialOfPlayWithLocalSearch mostOpenSpaceWithHighestScoreNthChildMatchingCondition

let playTrialsWithMaximalScoreExpectationLocalSearch =
    bestTrialOfPlayWithLocalSearch bestExpectedScoringNthChildMatchingCondition
