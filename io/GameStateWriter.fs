module Writer

open System
open System.IO
open System.Security

let writeSolutions path solutions =
    try
        File.WriteAllText(path, solutions)
    with
    | :? ArgumentException as e -> e.Message |> printfn "%s"
    | :? PathTooLongException as e -> e.Message |> printfn "%s"
    | :? DirectoryNotFoundException as e -> e.Message |> printfn "%s"
    | :? IOException as e -> e.Message |> printfn "%s"
    | :? UnauthorizedAccessException as e -> e.Message |> printfn "%s"
    | :? NotSupportedException as e -> e.Message |> printfn "%s"
    | :? SecurityException as e -> e.Message |> printfn "%s"
