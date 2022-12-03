module RuntimeBenchmark

open Writer

let measureAndPrintRuntime func arg =
    let startTime = System.DateTime.Now
    func arg
    let runTime = System.DateTime.Now - startTime
    runTime
    |> sprintf "%A"
    |> (+) "Runtime: "
    |> writeStringToConsole
