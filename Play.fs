module Play

open Game

let private tileInsertionOptions =
    seq {Exponent 1; Exponent 2}

let private insertRandomTile = 
    tileInsertionOptions
    |> GameState.addRandomTile

let moveLeft =
    GameState.shiftLeft
    >> insertRandomTile

let moveRight =
    GameState.shiftRight
    >> insertRandomTile

let moveUp =
    GameState.shiftUp
    >> insertRandomTile

let moveDown =
    GameState.shiftDown
    >> insertRandomTile

let makeInitialBoard rows cols numPreplacedTiles initialTileOptions =
    
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

let initialState = makeInitialBoard 4 4 2 initialTileOptions
