module LocalSearch.Scoring.Evaluators

open LocalSearch.Scoring.Scorers
open Game
open SearchTree
open TupleUtils

let private evaluateWithScorer scoreTree selectionFunction actionTreeInput =

    let actionTrees = Seq.cache actionTreeInput
    
    let actionScores =
        Seq.map (Seq.map scoreTree |> mapSnd)
        >> Seq.map (selectionFunction |> mapSnd)
    
    let bestScore =
        Seq.map snd
        >> Seq.max
    
    let bestActions actionScores =
        actionScores
        |> Seq.filter ((=) (bestScore actionScores) << snd)
        |> Seq.map fst
    
    let actionInBestActions action =
        actionTrees
        |> actionScores
        |> Seq.cache
        |> bestActions
        |> Seq.contains action
    
    actionTrees
    |> Seq.filter (fst >> actionInBestActions)

let evaluateWithRandomImprovedState actionTrees =

    let baseScore =
        actionTrees
        |> Seq.tryHead
        |> Option.map snd
        |> Option.map Seq.head
        |> Option.map SearchTree.rootOf
        |> Option.map (SearchTree.mapState GameState.scoreOf 0)
    
    let scoreIncreased baseScore tree =
        match baseScore with
        | Some i ->
            tree |> SearchTree.mapState GameState.scoreOf 0 > i
            |> Some
        | None -> None
    
    let countScoresIncreased =
        Seq.choose (scoreIncreased baseScore)
        >> Seq.filter id
        >> Seq.length
        |> mapSnd

    let weightAction (action, count) =
        Seq.replicate count action
    
    let randomWeightedAction weightedActions =
        match weightedActions |> Random.randomElementIfNonempty with
        | Some actionOption -> actionOption
        | None -> None

    actionTrees
    |> Seq.map countScoresIncreased
    |> Seq.collect weightAction
    |> randomWeightedAction

let evaluateWithMaxScore actionTrees = 
    evaluateWithScorer scoreByScore Seq.max actionTrees

let evaluateWithMaxBlanks actionTrees =
    evaluateWithScorer scoreByBlanks Seq.max actionTrees

let evaluateWithExpectedBlanks actionTrees =
    evaluateWithScorer scoreByBlanks Seq.average actionTrees

let evaluateWithExpectedScore actionTrees =
    evaluateWithScorer scoreByScore Seq.average actionTrees

let evaluateWithExpectedMonotonicity actionTrees =
    evaluateWithScorer scoreByMonotonicity Seq.average actionTrees

let evaluateWithExpectedUniformity actionTrees =
    evaluateWithScorer scoreByUniformity Seq.average actionTrees
