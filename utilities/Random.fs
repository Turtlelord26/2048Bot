module Random

open System

let random = Random()

type Random with
    member this.next max =
        this.Next(max)

///Will crash if given an empty sequence! 
///Use randomElementIfNonempty if the possibility exists
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
