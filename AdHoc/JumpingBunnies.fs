module JumpingBunnies
open System
open System.Numerics

let rec gcd a b = 
    if b = 0I then a
    else gcd b (a%b)

let lcm a b = a * b / gcd a b

let Run() = 
    let bunniesCount = Console.ReadLine() |> int
    let results = 
        Console.ReadLine().Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |>
        Array.map(fun x -> BigInteger.Parse(x)) |>
        Array.fold(fun acc x -> lcm acc x) 1I
        
    printfn "%A" results

Run()