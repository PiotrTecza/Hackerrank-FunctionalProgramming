module RecursiveTrees
open System

let printArray (arr:char[,]) = 
    for r in arr.GetLength(0)-1..-1..0 do
        for c in 0..arr.GetLength(1)-1 do
            printf "%c" arr.[r,c]
        printf "%s" Environment.NewLine

let rec createTree startColumn startRow length level (arr:char[,]) :  char[,]=
    if level > 0 then   
        for i in 0..length-1 do
            arr.[startRow + i, startColumn] <- '1'
        for i in 0..length-1 do
            arr.[startRow + length + i, startColumn - i - 1] <- '1'
            arr.[startRow + length + i, startColumn + i + 1] <- '1'
        createTree (startColumn-length) (startRow+length*2) (length/2) (level-1) arr |> ignore
        createTree (startColumn+length) (startRow+length*2) (length/2) (level-1) arr |> ignore
    arr

Array2D.init 63 100 (fun i j -> '_') |> createTree 49 0 16 (Console.ReadLine()|>int) |> printArray
