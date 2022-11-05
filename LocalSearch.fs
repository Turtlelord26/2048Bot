module LocalSearch

open Average
open Game
open Moves
open Operators
open Random
open Search

let private nthChildren depth =

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

let private chooseActionByRandomImprovedScore depth state =

    let startingScore = GameState.scoreOf state

    let isScoreIncreased node =
        SearchTree.scoreOf node > startingScore
    
    let chooseByRandomImprovedScore =
        Seq.filter isScoreIncreased
        >> randomElementIfNonempty
        >>= SearchTree.getRootCauseAction

    state
    |> nthChildren depth
    |> chooseByRandomImprovedScore

let private bestActionByBestScore depth =

    let chooseByBestScore trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> Seq.groupBy SearchTree.scoreOf
            |> Seq.maxBy fst
            |> snd
            |> randomElement
            |> SearchTree.getRootCauseAction

    nthChildren depth
    >> chooseByBestScore

let private bestActionByMostOpenSpaces depth =

    let chooseByMostOpenSpaces trees =
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
            |> randomElement
            |> SearchTree.getRootCauseAction
    
    nthChildren depth
    >> chooseByMostOpenSpaces

let private bestActionByMostOpenSpaceWithHighestScore depth =

    let chooseByMostOpenSpacesWithHighestScore trees =
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
            |> Seq.groupBy SearchTree.scoreOf
            |> Seq.maxBy fst
            |> snd
            |> randomElement
            |> SearchTree.getRootCauseAction
    
    nthChildren depth
    >> chooseByMostOpenSpacesWithHighestScore

let private bestActionByExpectedScoreLocalSearch depth =
    
    let expectedScoreOf =
        Seq.map SearchTree.scoreOf
        >> Seq.map RunningAverage.construct
        >> Seq.sum
        >> RunningAverage.consume

    let chooseByBestScoreExpectation trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> Seq.groupBy SearchTree.getRootCauseAction
            |> Seq.maxBy (snd >> expectedScoreOf)
            |> fst
    
    nthChildren depth
    >> chooseByBestScoreExpectation

let private playWithSearch determineNextAction lookaheadDepth =

    let rec randomLocalSearchIteration lookaheadDepth actionsTaken (vstate: ValidatedGameState) =
        match vstate |> ValidatedGameState.statusOf with
        | Valid ->
            match 
                vstate
                |> ValidatedGameState.stateOf
                |> determineNextAction lookaheadDepth
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
    
    randomLocalSearchIteration lookaheadDepth []

let private bestTrialOfPlayWithSearch search trials lookaheadDepth initialState =

    let score =
        fst
        >> ValidatedGameState.stateOf
        >> GameState.scoreOf

    seq {for _ in 1..trials do playWithSearch search lookaheadDepth initialState}
    |> Seq.maxBy score

let playTrialsWithRandomLocalSearch =
    bestTrialOfPlayWithSearch chooseActionByRandomImprovedScore

let playTrialsWithMaximalLocalSearch =
    bestTrialOfPlayWithSearch bestActionByBestScore

let playTrialsWithMaximalBlanksLocalSearch =
    bestTrialOfPlayWithSearch bestActionByMostOpenSpaces

let playTrialsWithMaximalBlanksThenScoreLocalSearch =
    bestTrialOfPlayWithSearch bestActionByMostOpenSpaceWithHighestScore

let playTrialsWithMaximalScoreExpectationLocalSearch =
    bestTrialOfPlayWithSearch bestActionByExpectedScoreLocalSearch
