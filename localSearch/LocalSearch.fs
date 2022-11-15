module LocalSearch

open LocalSearchActions
open LocalSearchScoring
open LocalSearchUtils
open Operators

let playTrials depth scoringFunction =
    
    let searchFunction =
        nthChildren depth
        >> scoringFunction
    
    bestTrialOfPlayWithSearch searchFunction returnFromTerminalState
