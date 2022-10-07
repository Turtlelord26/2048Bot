module Milestone1

open Game
open Search

let private addTwoTileByDeterministicVerticalScan (State (tiles, score)) =
    let mutable notYetAdded = true
    let newTiles = Array2D.copy tiles
    for row in 0..3 do
        for col in 0..3 do
            if newTiles[row,col] = Blank && notYetAdded then 
                newTiles[row,col] <- Exponent 1
                notYetAdded <- false
    State (newTiles, score)

let private move direction =
    direction
    >> addTwoTileByDeterministicVerticalScan

let private moveLeft = move GameState.shiftLeft

let private moveRight = move GameState.shiftRight

let private moveUp = move GameState.shiftUp

let private moveDown = move GameState.shiftDown

let private shiftState parent state action =
    match action with
    | Root -> Tree (state, Root, parent, Empty, Empty, Empty, Empty)
    | Left -> Tree (state |> moveLeft, Left, parent, Empty, Empty, Empty, Empty)
    | Right -> Tree (state |> moveRight, Right, parent, Empty, Empty, Empty, Empty)
    | Up -> Tree (state |> moveUp, Up, parent, Empty, Empty, Empty, Empty)
    | Down -> Tree (state |> moveDown, Down, parent, Empty, Empty, Empty, Empty)

let comprehensiveStateSearchWithDeterministicTileAddition =

    let expandNode tree =
        match tree with
        | (Tree (state, action, parent, _, _, _, _)) ->
            let child = shiftState tree state
            Tree (state, action, parent, child Left, child Right, child Up, child Down)
        | Empty ->
            Empty

    let expansionsOf =
        expandNode >> SearchTree.getChildren

    let pathString =
        SearchTree.pathToRoot
        >> List.map SearchTree.actionOf
        >> Action.manyToString
    
    let scoreAndMoves node =
        (node |> SearchTree.scoreOf |> string) + (node |> pathString)
    (*
    let debugPrintAllLookaheads states =
        let debugPrint node =
            let printNodeState tree =
                match tree with
                | Empty -> ()
                | Tree (state, _, _, _, _, _, _) -> state |> TestUtils.printGameState
            scoreAndMoves node |> printfn "%s"
            printNodeState node
        Seq.iter debugPrint states
        states
    *)
    let searchAheadThreeForHighestScore =
        SearchTree.toSearchTreeRoot
        >> expansionsOf
        >> Seq.map expansionsOf
        >> Seq.concat
        >> Seq.map expansionsOf
        >> Seq.concat
        //>> debugPrintAllLookaheads
        >> Seq.maxBy SearchTree.scoreOf
    
    searchAheadThreeForHighestScore
    >> scoreAndMoves
