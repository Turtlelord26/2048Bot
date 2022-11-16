module Initialization

open Game

let private makeInitialBoard rows cols numPreplacedTiles initialTileOptions =
    
    let rec addRandomTiles options count state =
        match count with
        | i when i > 0 ->
            state
            |> GameState.addRandomTile options
            |> addRandomTiles options (count - 1)
        | _ -> state
    
    let addRandomInitialTiles = addRandomTiles initialTileOptions

    GameState.initialState rows cols
    |> addRandomInitialTiles numPreplacedTiles

let private initialTileOptions = seq {Exponent 1}

let initialState =
    initialTileOptions
    |> makeInitialBoard 4 4 2