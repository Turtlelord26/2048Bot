module LocalSearch

open Average
open Game
open LocalSearchActions
open LocalSearchUtils
open Operators
open Random
open Search

let playTrialsWithRandomLocalSearch depth =

    let isScoreIncreased state tree =
            SearchTree.scoreOf tree > GameState.scoreOf state
        
    let chooseByRandomImprovedScore state =
        Seq.filter (isScoreIncreased state)
        >> randomElementIfNonempty
        >>= SearchTree.getRootCauseAction

    let chooseActionByRandomImprovedScore state =
        state
        |> nthChildren depth
        |> (chooseByRandomImprovedScore state) 

    bestTrialOfPlayWithSearch chooseActionByRandomImprovedScore returnFromTerminalState

let playTrialsWithMaximalLocalSearch depth =

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

    let bestActionByBestScore =
        nthChildren depth
        >> chooseByBestScore

    bestTrialOfPlayWithSearch bestActionByBestScore returnFromTerminalState

let playTrialsWithMaximalBlanksLocalSearch depth =

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
    
    let bestActionByMostOpenSpaces =
        nthChildren depth
        >> chooseByMostOpenSpaces

    bestTrialOfPlayWithSearch bestActionByMostOpenSpaces returnFromTerminalState

let playTrialsWithMaximalBlanksThenScoreLocalSearch depth =

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
    
    let bestActionByMostOpenSpaceWithHighestScore =
        nthChildren depth
        >> chooseByMostOpenSpacesWithHighestScore

    bestTrialOfPlayWithSearch bestActionByMostOpenSpaceWithHighestScore returnFromTerminalState

let playTrialsWithMaximalScoreExpectationLocalSearch depth =

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
    
    let bestActionByExpectedScoreLocalSearch =
        nthChildren depth
        >> chooseByBestScoreExpectation

    bestTrialOfPlayWithSearch bestActionByExpectedScoreLocalSearch returnFromTerminalState
