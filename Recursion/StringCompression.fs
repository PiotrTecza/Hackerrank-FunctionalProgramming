module StringCompression
open System

let compress (str:string) = 
    let mutable previousChar = str.[0]
    let mutable counter = 0

    for c in str do
        if c = previousChar then
            counter <- counter + 1
        else
            printf "%c" previousChar
            if counter > 1 then 
                printf "%i" counter
            previousChar <- c
            counter <- 1

    printf "%c" previousChar
    if counter > 1 then 
        printf "%i" counter
    |> ignore

Console.ReadLine() |> string |> compress
