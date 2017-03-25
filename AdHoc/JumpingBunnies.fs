module JumpingBunnies

let rec gcd (a:bigint) (b:bigint) = 
    if b = 0I then a
    else gcd b (a%b)

let lcm (a:bigint) (b:bigint) = a * b / gcd a b
