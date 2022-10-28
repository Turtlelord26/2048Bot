module Random

open System

let random = Random()

type Random with
    member this.next max =
        this.Next(max)