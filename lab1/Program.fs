open System

let eps = 10.0**(-9.0)
let a = 0.0
let b = 0.5
let step = 0.05

let builtin (x: float) =
    let nextTerm n =
        let term = -(1.0 + 2.0 / (3.0 ** float (n + 1))) * (x ** float n)
        Some(term, n + 1)
    
    let terms = Seq.unfold nextTerm 0 |> Seq.takeWhile (fun t -> abs t > eps) |> Seq.toList
    (Seq.sum terms, List.length terms)

let smarttaylor (x: float) =
    let mutable part1 = -1.0
    let mutable part2 = -2.0 / 3.0
    let mutable result = 0.0
    let mutable count = 0
    let mutable element = part1 + part2

    while abs(part1 + part2) > eps do
        result <- result + part1 + part2 
        count <- count + 1
        part1 <- part1 * x            
        part2 <- part2 * (x / 3.0)
        element <- part1 + part2  
    (result, count)

let dumbtaylor (x: float) =
    let mutable result = 0.0
    let mutable n = 1.0
    let mutable element = -(1.0 + 2.0 / 3.0)
    let mutable count = 0
    
    while abs(element) > eps do
        result <- result + element
        count <- count + 1
        element <- -(1.0 + 2.0 / (3.0**(n + 1.0)))*(x**n)
        n <- n + 1.0
    (result, count)

let func1 x =
    (3.0 * x - 5.0) / (x**2.0 - 4.0 * x + 3.0)

let func2 x =
    (sin x)**2.0

let func3 x =
    (1.0 - x**2.0 / 2.0)*(cos x) - (x / 2.0) * sin x

let bisectionmethod (starta: float) (startb:float) f =
    let mutable a = starta
    let mutable b = startb
    let mutable mid = 0.0
    let mutable flag = false

    if (f a) * (f b) > 0.0 then
        nan
    else
        while not flag && abs(a - b) > eps do
            while Double.IsInfinity(f a) || Double.IsNaN(f a) do
                    a <- a + eps
            while Double.IsInfinity(f b) || Double.IsNaN(f b) do
                    b <- b - eps
            mid <- (a + b) / 2.0

            if (f mid) = 0.0 then
                flag <- true
            elif (f a) * (f mid) < 0.0 then
                b <- mid
            else
                a <- mid

            printf "%f\n" mid
        
        mid

let task1 =
    printfn "| %8s | %12s | %12s | %8s | %12s | %8s |" "x" "Builtin" "Smart Taylor" "# terms" "Dumb Taylor" "# terms"
    printfn "|%s|%s|%s|%s|%s|%s|" (String.replicate 10 "-") (String.replicate 14 "-") (String.replicate 14 "-") (String.replicate 10 "-") (String.replicate 14 "-") (String.replicate 10 "-")
    
    let mutable i = a
    while i <= b + eps do
        let (dumb, dumbTerms) = dumbtaylor i
        let (smart, smartTerms) = smarttaylor i
        let (built, builtTerms) = builtin i
        
        printfn "| %8.4f | %12.9f | %12.9f | %8d | %12.9f | %8d |" 
            i built smart smartTerms dumb dumbTerms
        i <- i + step

[<EntryPoint>]
let main argv =
    //task1

    printf "\n\n"
    bisectionmethod 1.0 4.0 func1
    printf "\n\n"
    bisectionmethod -1.0 15.0 func2
    printf "\n\n"
    bisectionmethod 0.0 0.1 func3
    0

    // написать цикл, где мы начинаем с малого интервала от 0 до eps и начинаем его двигать (?)