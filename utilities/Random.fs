module Random

open System

let random = Random()

type Random with
    member this.next max =
        this.Next(max)
    
    member this.nextFloat = this.NextDouble()

/// Will crash if given an empty sequence! 
/// Use randomElementIfNonempty if the possibility exists
let randomElement sequence =

    let randomIndex =
        sequence
        |> Seq.length
        |> random.next
    
    sequence
    |> Seq.item randomIndex

let randomElementIfNonempty sequence =
    if
        sequence
        |> Seq.isEmpty
    then
        None
    else
        sequence
        |> randomElement
        |> Some

/// Elements of the argument sequence must have weights that sum to 1.
/// After processing weights totaling to 1 further elements are ignored.
/// Will crash if given an empty sequence.
let weightedRandomElement sequence =

    let rec selectByWeight selector sequence = 
        match Seq.head sequence with
        | i, element when i > selector ->
            element
        | j, _ ->
            selectByWeight (selector - j) (Seq.tail sequence)
    
    selectByWeight (random.nextFloat) sequence
