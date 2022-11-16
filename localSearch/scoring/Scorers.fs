module LocalSearch.Scoring.Scorers

open Game
open SearchTree

let scoreByScore =
    SearchTree.mapState GameState.scoreOf 0
    >> float

let scoreByBlanks =

    let countBlankTiles =
        GameState.boardOf
        >> Board.getBlankTiles
        >> Seq.length

    SearchTree.mapState countBlankTiles 0
    >> float

let scoreByMonotonicity =

    let rows =
        GameState.boardOf
        >> Board.getRows
        >> List.map (Array.map Tile.score)

    let cols =
        GameState.boardOf
        >> Board.getCols
        >> List.map (Array.map Tile.score)
    
    let rowsAndCols state = 
        (state |> rows, state |> cols)
    
    let comparators = 
        List.allPairs [(<=); (>=)] [(>=); (<=)]
    
    let scoreLinearMonotonicity comp =
        Seq.pairwise
        >> Seq.map comp
        >> Seq.length
    
    let scoreAllLinearMonotonicity comp =
        Seq.map (scoreLinearMonotonicity comp)
        >> Seq.sum
    
    let scoreCornerMonotonicity rows cols (rowComp, colComp) =
        scoreAllLinearMonotonicity rowComp rows
        + scoreAllLinearMonotonicity colComp cols

    let scoreMaxCornerMonotonicity (rows, cols) =
        Seq.map (scoreCornerMonotonicity rows cols)
        >> Seq.max
    
    let scoreStateMonotonicity state =
        scoreMaxCornerMonotonicity (rowsAndCols state) comparators 
    
    SearchTree.mapState scoreStateMonotonicity 0
    >> float

let scoreByUniformity =

    let tileCounts = 
        GameState.boardOf
        >> Board.getTiles
        >> Seq.filter (not << Tile.isBlank)
        >> Seq.countBy Tile.score
        >> Seq.map snd
    
    let cubeCount =
        float
        >> ( ** ) 3.0
    
    let uniformity =
        tileCounts
        >> Seq.map cubeCount
        >> Seq.fold (+) 0.
    
    SearchTree.mapState uniformity 0.
