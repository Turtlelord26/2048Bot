open Game
open Random
open Search
open Test
open TupleUtils
open Writer

let tests () =
    testBasicShifts
    testOrderMatters
    testOrderMatters2
    testPackedBoard
    testRightUp
    testUp2
    testUnlikePacked
    testMilestone1SampleFirstMove
    testMilestone1SampleSecondMove
    testRead false

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

let insertRandomTile = 
    seq {Exponent 1; Exponent 2}
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

let chooseNthChildMatchingCondition selectChild depth condition =

    let rec searchToDepth depths =
        match depths with
        | i when i > 1 ->
            Seq.map SearchTree.expandNodeWithExhaustiveInsertion
            >> Seq.map SearchTree.getChildren
            >> Seq.concat
            >> searchToDepth (depths - 1)
        | 1 ->
            Seq.map SearchTree.expandNodeWithoutInsertion
            >> Seq.map SearchTree.getChildren
            >> Seq.concat
        | 0 ->
            id
        | _ ->
            fun _ -> Seq.empty

    SearchTree.toSearchTreeRoot
    >> Seq.replicate 1
    >> searchToDepth depth
    >> Seq.filter condition
    >> selectChild

let randomNthChildMatchingCondition =
    randomElementIfNonempty
    |> chooseNthChildMatchingCondition

let bestNthChildMatchingCondition =

    let chooseBest trees =
        if
            trees
            |> Seq.isEmpty
        then
            None
        else
            trees
            |> Seq.maxBy SearchTree.scoreOf
            |> Some

    chooseBest
    |> chooseNthChildMatchingCondition

let playWithLocalSearch localSearch lookaheadDepth moves state = //CORRECTION!! REMOVE MOVES PARAMETER. GAMES RUN TO TERMINATION.
                                                                 //MILESTONE VARIABLE N IS HOW MANY TIMES THE WHOLE GAME IS TO BE RUN
                                                                 //AND OUTPUT THE BEST TRIAL OF THE N RUNS.
                                                                 //SO FOR NOW DO ONE RUN AND AFTER VERIFICATION STEPS RUN IT THE FULL N TIMES.

    let localSearchLookahead lookaheadDepth state =

        let (State (_, startingScore)) = state

        let isScoreIncreased node =
            SearchTree.scoreOf node > startingScore
        
        state
        |> localSearch lookaheadDepth isScoreIncreased
    
    let getNextAction =
        Seq.item 1
        >> SearchTree.actionOf
    
    let determineAction lookaheadDepth =
        localSearchLookahead lookaheadDepth
        >> Option.map SearchTree.pathToRoot
        >> Option.map getNextAction

    let rec randomLocalSearchIteration lookaheadDepth stepsLeft actionsTaken state =
        match stepsLeft with
        | i when i > 0 ->
            match state |> determineAction lookaheadDepth with
            | Some Left -> state |> moveLeft |> randomLocalSearchIteration lookaheadDepth (stepsLeft - 1) (Left :: actionsTaken)
            | Some Right -> state |> moveRight |> randomLocalSearchIteration lookaheadDepth (stepsLeft - 1) (Right :: actionsTaken)
            | Some Up -> state |> moveUp |> randomLocalSearchIteration lookaheadDepth (stepsLeft - 1) (Up :: actionsTaken)
            | Some Down -> state |> moveDown |> randomLocalSearchIteration lookaheadDepth (stepsLeft - 1) (Down :: actionsTaken)
            | Some Root
            | None ->
                state, actionsTaken
        | _ ->
            state, actionsTaken
    
    state
    |> randomLocalSearchIteration lookaheadDepth moves []
    |> mapSnd List.rev

let playWithRandomLocalSearch = playWithLocalSearch randomNthChildMatchingCondition

let playWithMaximalLocalSearch = playWithLocalSearch bestNthChildMatchingCondition

let initialTileOptions = seq {Exponent 1}

let initialState = makeInitialBoard 4 4 2 initialTileOptions

[<EntryPoint>]
let main args =
    match args with
    | [||] ->
        tests ()
        0
    | [|"randomLocalSearch"|] ->
        initialState
        |> playWithRandomLocalSearch 2 100
        ||> writeResult
        0
    | [|"maximalLocalSearch"|] ->
        initialState
        |> playWithMaximalLocalSearch 2 25
        ||> writeResult
        0
    | _ ->
        1
