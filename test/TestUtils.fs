module TestUtils

let passfail assertion =
    match assertion with
    | true -> "Pass"
    | false -> "Fail"

let printTest preface =
    passfail
    >> (+) preface
    >> printfn "%s"

let executeTest preface initialState testedFunc evalFunc visualizeFunc =

    let actualState =
        initialState
        |> testedFunc
    
    let pass = evalFunc actualState
    
    if not pass then 
        visualizeFunc preface initialState actualState

    pass
    |> printTest preface