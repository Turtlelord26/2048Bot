module Initialization

open Game

let makeInitialBoard rows cols numPreplacedTiles initialTileOptions =
    
    let rec addRandomTiles options count state =
        match count with
        | i when i > 0 ->
            state
            |> GameState.addRandomTile options
            |> addRandomTiles options (count - 1)
        | _ -> state

    GameState.initialState rows cols
    |> addRandomTiles initialTileOptions numPreplacedTiles
