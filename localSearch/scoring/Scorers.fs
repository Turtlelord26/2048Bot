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

    let cols =
        GameState.boardOf
        >> Board.getCols
    
    let rowsAndCols state = 
        (state |> rows, state |> cols)
    
    let comparators = ((<=), (>=))
    
    let scoreLinearMonotonicity comp =
        Seq.pairwise
        >> Seq.map comp
        >> Seq.length
    
    let scoreAllLinearMonotonicity comp =
        Seq.map (scoreLinearMonotonicity comp)
        >> Seq.sum
    
    let scoreCornerMonotonicity rows rowComp cols colComp =
        scoreAllLinearMonotonicity rowComp rows
        + scoreAllLinearMonotonicity colComp cols

    let scoreMaxCornerMonotonicity (comp1, comp2) (rows, cols) =
        seq {scoreCornerMonotonicity rows comp1 cols comp1;
             scoreCornerMonotonicity rows comp1 cols comp2;
             scoreCornerMonotonicity rows comp2 cols comp1;
             scoreCornerMonotonicity rows comp2 cols comp2}
        |> Seq.max
    
    let scoreStateMonotonicity =
        rowsAndCols
        >> scoreMaxCornerMonotonicity comparators 
    
    SearchTree.mapState scoreStateMonotonicity 0
    >> float

let scoreByUniformity =

    let tileCounts = 
        GameState.boardOf
        >> Board.getTiles
        >> Seq.countBy id
        >> Seq.map snd
    
    let cubeCount =
        float
        >> ( ** ) 3.0
    
    let uniformity =
        tileCounts
        >> Seq.map cubeCount
        >> Seq.fold (+) 0.
    
    SearchTree.mapState uniformity 0.
