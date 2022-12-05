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

    let cached = Seq.cache sequence

    let randomIndex =
        cached
        |> Seq.length
        |> random.next
    
    cached
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
