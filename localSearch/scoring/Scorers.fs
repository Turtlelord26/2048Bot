module LocalSearch.Scoring.Scorers

open Game
open SearchTree

let scoreByScore =
    SearchTree.mapState GameState.scoreOf 0
    >> double

let scoreByBlanks =

    let countBlankTiles =
        GameState.boardOf
        >> Board.getBlankTiles
        >> Seq.length

    SearchTree.mapState countBlankTiles 0
    >> double

//TODO: scoreByMonotonicity

//TODO: scoreByUniformity
