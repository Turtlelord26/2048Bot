module LocalSearch.Selection

open LocalSearch.Scoring.Evaluators
open Random
open SearchTree

let private chooseByEvaluator evaluator =

    let hasAction (actionOption, seqTree) =
        match actionOption with
        | Some action -> Some (action, seqTree)
        | None -> None
    
    Seq.groupBy SearchTree.getRootCauseAction
    >> Seq.choose hasAction
    >> evaluator
    >> Option.map fst

let private makeEvaluator terminalEvaluator nonterminalEvaluators =
    nonterminalEvaluators
    |> Seq.fold (>>) id
    >> terminalEvaluator

let chooseByRandomImprovement actionTrees =
    actionTrees
    |> Seq.groupBy SearchTree.getRootCauseAction
    |> evaluateWithRandomImprovedState

let chooseByBestScore actionTrees =

    let evaluator =
        [evaluateWithMaxScore]
        |> makeEvaluator randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator

let chooseByMostOpenSpaces actionTrees =

    let evaluator =
        [evaluateWithMaxBlanks]
        |> makeEvaluator randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator

let chooseByMostOpenSpacesWithHighestScore actionTrees =

    let evaluator =
        [evaluateWithMaxBlanks;
         evaluateWithMaxScore]
        |> makeEvaluator randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator

let chooseByBestScoreExpectation actionTrees =

    let evaluator = makeEvaluator randomElementIfNonempty [evaluateWithExpectedScore]
    
    actionTrees
    |> chooseByEvaluator evaluator

let chooseByEUMR actionTrees =

    let evaluator =
        [evaluateWithExpectedBlanks;
         evaluateWithExpectedUniformity;
         evaluateWithExpectedMonotonicity]
        |> makeEvaluator randomElementIfNonempty
    
    actionTrees
    |> chooseByEvaluator evaluator
