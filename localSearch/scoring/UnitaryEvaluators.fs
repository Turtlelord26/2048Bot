module LocalSearch.Scoring.Evaluators

open LocalSearch.Scoring.Scorers
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

let evaluateWithExpectedBlanks actionTrees =
    evaluateWithScorer scoreByBlanks Seq.average actionTrees

let evaluateWithExpectedScore actionTrees =
    evaluateWithScorer scoreByScore Seq.average actionTrees

let evaluateWithExpectedMonotonicity actionTrees =
    evaluateWithScorer scoreByMonotonicity Seq.average actionTrees

let evaluateWithExpectedUniformity actionTrees =
    evaluateWithScorer scoreByUniformity Seq.average actionTrees
