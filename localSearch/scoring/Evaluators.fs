module LocalSearch.Scoring.Evaluators

open LocalSearch.Scoring.Scorers
open TupleUtils

let private evaluateWithScorer scoreTree selectionFunction actionTrees =
    
    let actionScores =
        actionTrees
        |> Seq.map (Seq.map scoreTree |> mapSnd)
        |> Seq.map (selectionFunction |> mapSnd)
    
    let bestScore =
        actionScores
        |> Seq.map snd
        |> Seq.max
    
    let bestActions =
        actionScores
        |> Seq.filter (snd >> (fun score -> score = bestScore))
        |> Seq.map fst
    
    let actionInBestActions action =
        Seq.contains action bestActions
    
    actionTrees
    |> Seq.filter (fst >> actionInBestActions)

let evaluateWithMaxScore actionTrees = 
    evaluateWithScorer scoreByScore Seq.max actionTrees

let evaluateWithMaxBlanks actionTrees =
    evaluateWithScorer scoreByBlanks Seq.max actionTrees

let evaluateWithExpectedScore actionTrees =
    evaluateWithScorer scoreByScore Seq.average actionTrees

//TODO: evaluateByMonotonicity

//TODO: evaluateByUniformity
