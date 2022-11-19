module LocalSearch.Play

open Game
open LocalSearch.Actions
open LocalSearch.Search

let private playWithSearch localSearch onEndOfGame: GameState -> GameState * seq<SearchTree.Action> =
    localSearch onEndOfGame

let private bestTrialOfPlayWithSearch searchFunction returnFromTerminalState trials initialState =

    let recurringSearch = searchActionsUntilTermination searchFunction

    let localSearch = playWithSearch recurringSearch returnFromTerminalState

    let score =
        fst
        >> GameState.scoreOf

    seq {for _ in 1..trials do localSearch initialState}
    |> Seq.maxBy score

let playTrials tileInsertionOptions depth scoringFunction trials initialState =

    let searchFunction =
        nthChildren tileInsertionOptions depth
        >> scoringFunction
    
    bestTrialOfPlayWithSearch searchFunction returnFromTerminalState trials initialState
