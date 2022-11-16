module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.Search

let private playWithSearch searchAndExecuteNextAction onVictory onDefeat =

    let rec randomLocalSearchIteration actionsTaken vstate =
        match vstate |> ValidatedGameState.statusOf with
        | Valid ->
            searchAndExecuteNextAction randomLocalSearchIteration vstate actionsTaken
        | Victory ->
            onVictory vstate actionsTaken
        | Defeat ->
            onDefeat vstate actionsTaken
    
    randomLocalSearchIteration []

let private bestTrialOfPlayWithSearch searchMethod returnFromTerminalState trials initialState =

    let iterateLocalSearch = searchAndExecuteNextAction searchMethod

    let localSearch = playWithSearch iterateLocalSearch returnFromTerminalState returnFromTerminalState

    let score =
        fst
        >> ValidatedGameState.stateOf
        >> GameState.scoreOf

    seq {for _ in 1..trials do localSearch initialState}
    |> Seq.maxBy score

let playTrials depth scoringFunction trials initialState =

    let searchFunction =
        nthChildren depth
        >> scoringFunction
    
    bestTrialOfPlayWithSearch searchFunction returnFromTerminalState trials initialState
