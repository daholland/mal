// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let READ a = a
let EVAL a = a
let PRINT a = a

let REP =
    let mutable continueRead = true
    while continueRead do
        printf "user> "
        let input = Console.ReadLine()
        match input with
        | null -> continueRead <- false
        | inputString -> inputString |> READ |> EVAL |> PRINT |> printfn "%s"


[<EntryPoint>]
let main argv =
    REP
    0 // return an integer exit code
