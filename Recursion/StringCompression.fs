module StringCompression
open System

let builder = Text.StringBuilder()

let groupChars xs = 
    Seq.foldBack (fun x acc -> 
            match acc, x with
            | [], _ -> [(x,1)]
            | (c,n) :: rest, x when c = x -> (c, n+1) :: rest
            | acc, x -> (x,1) :: acc) xs []

let concatGroupus groups = 
    List.iter (fun g ->
        match g with
        | (c:char,1) -> builder.AppendFormat("{0}",c)
        | (c,n) -> builder.AppendFormat("{0}{1}",c,n)
        |> ignore) groups
    builder.ToString()

Console.ReadLine() |> groupChars |> concatGroupus |> printfn "%s"