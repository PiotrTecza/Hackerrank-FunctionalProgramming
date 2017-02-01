module StringCompression
open System

let compress str = 
    let mutable previousChar = '\000'
    let mutable counter = 0
    let mutable result = ""

    for c in str do
        if c = previousChar then
            counter <- counter + 1
        else
            result <- result + previousChar.ToString()
            if counter > 1 then 
                result <- result + counter.ToString()
            previousChar <- c
            counter <- 1

    result <- result + previousChar.ToString()
    if counter > 1 then 
        result <- result + counter.ToString()

    result

let Run() = 
    let input = Console.ReadLine() |> string
    let result = compress input
    printf "%s" result

Run()


