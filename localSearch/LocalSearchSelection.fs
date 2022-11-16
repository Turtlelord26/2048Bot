module LocalSearch.Selection

open LocalSearch.Scoring.Evaluators
open Random
open SearchTree

let private chooseByEvaluator evaluator =
    Seq.groupBy SearchTree.getRootCauseAction
    >> evaluator
    >> Option.map fst

let chooseByBestScore actionTrees =

    let evaluator =
        evaluateWithMaxScore
        >> randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator

let chooseByMostOpenSpaces actionTrees =

    let evaluator =
        evaluateWithMaxBlanks
        >> randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator

let chooseByMostOpenSpacesWithHighestScore actionTrees =

    let evaluator =
        evaluateWithMaxBlanks
        >> evaluateWithMaxScore
        >> randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator

let chooseByBestScoreExpectation actionTrees =

    let evaluator =
        evaluateWithExpectedScore
        >> randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator

let chooseByEUMR actionTrees =

    let evaluator =
        evaluateWithExpectedBlanks
        >> evaluateWithExpectedUniformity
        >> evaluateWithExpectedMonotonicity
        >> randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator
