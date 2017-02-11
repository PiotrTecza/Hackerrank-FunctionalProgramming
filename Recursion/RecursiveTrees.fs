module RecursiveTrees
open System

let arr = Array2D.init 20 20 (fun i j -> '_') 
let printArray (arr:char[,]) = 
    for r in arr.GetLength(0)-1..-1..0 do
        for c in 0..arr.GetLength(1)-1 do
            printf "%c" arr.[r,c]
        printf "%s" Environment.NewLine

printArray arr
let createTree startColumn startRow length (arr:char[,]) =
    for i in 0..length-1 do
        arr.[startRow + i, startColumn] <- '1'
    for i in 0..length-1 do
        arr.[startRow + length + i, startColumn - i] <- '1'
        arr.[startRow + length + i, startColumn + i] <- '1'
    arr

arr |> createTree 10 0 8 |> printArray
