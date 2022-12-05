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

let chooseByBestScoreExpectation trees =

    let evaluator = makeEvaluator randomElementIfNonempty [evaluateWithExpectedScore]
    
    trees
    |> chooseByEvaluator evaluator

let chooseByEUMR trees =

    let evaluator =
        [evaluateWithExpectedBlanks;
         evaluateWithExpectedUniformity;
         evaluateWithExpectedMonotonicity]
        |> makeEvaluator randomElementIfNonempty
    
    trees
    |> chooseByEvaluator evaluator
