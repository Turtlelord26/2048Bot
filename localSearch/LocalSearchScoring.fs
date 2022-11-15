module LocalSearchScoring

open Average
open Random
open Search

let chooseByBestScore trees =
    if
        trees
        |> Seq.isEmpty
    then
        None
    else
        trees
        |> Seq.groupBy SearchTree.scoreOf
        |> Seq.maxBy fst
        |> snd
        |> randomElement
        |> SearchTree.getRootCauseAction

let chooseByMostOpenSpaces trees =
    if
        trees
        |> Seq.isEmpty
    then
        None
    else
        trees
        |> Seq.groupBy SearchTree.countBlanks
        |> Seq.maxBy fst
        |> snd
        |> randomElement
        |> SearchTree.getRootCauseAction

let chooseByMostOpenSpacesWithHighestScore trees =
    if
        trees
        |> Seq.isEmpty
    then
        None
    else
        trees
        |> Seq.groupBy SearchTree.countBlanks
        |> Seq.maxBy fst
        |> snd
        |> Seq.groupBy SearchTree.scoreOf
        |> Seq.maxBy fst
        |> snd
        |> randomElement
        |> SearchTree.getRootCauseAction

let chooseByBestScoreExpectation trees =

    let expectedScoreOf =
        Seq.map SearchTree.scoreOf
        >> Seq.map RunningAverage.construct
        >> Seq.sum
        >> RunningAverage.consume

    if
        trees
        |> Seq.isEmpty
    then
        None
    else
        trees
        |> Seq.groupBy SearchTree.getRootCauseAction
        |> Seq.maxBy (snd >> expectedScoreOf)
        |> fst
