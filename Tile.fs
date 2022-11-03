namespace Game

type Tile =
    | Exponent of int
    | Blank
    with

    static member isBlank tile =
        tile = Blank
    
    static member private scoreValue =
        float
        >> ( ** ) 2.
        >> int
    
    static member score tile =
        match tile with
        | Exponent value -> value |> Tile.scoreValue
        | Blank -> 0
