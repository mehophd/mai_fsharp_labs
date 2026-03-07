open System

let eps = 10.0**(-9.0)
let maxiter = 10000 

let func1 x = (3.0 * x - 5.0) / (x**2.0 - 4.0 * x + 3.0)
let func2 x = (sin x)**2.0
let func3 x = (1.0 - x**2.0 / 2.0)*(cos x) - (x / 2.0) * sin x

let g1 x = x - 0.01 * func1 x
let g2 x = x - 0.01 * func2 x  
let g3 x = x - 0.01 * func3 x 

let deriv1 x = 
    let num = 3.0 * (x**2.0 - 4.0*x + 3.0) - (3.0*x - 5.0)*(2.0*x - 4.0)
    let den = (x**2.0 - 4.0*x + 3.0)**2.0
    num / den

let deriv2 x = 2.0 * sin x * cos x

let deriv3 x = 
    let term1 = (-x) * cos x
    let term2 = (1.0 - x**2.0/2.0) * (-sin x)
    let term3 = -0.5 * sin x
    let term4 = -(x/2.0) * cos x
    term1 + term2 + term3 + term4

let bisectionmethod (starta: float) (startb: float) f =
    let mutable a = starta
    let mutable b = startb
    let mutable mid = 0.0
    let mutable flag = false

    if (f a) * (f b) > 0.0 then
        (nan, -1)
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

        (mid, -1)

let iterationmethod (x0: float) f g =
    let mutable x = x0
    let mutable i = 0
    let maxcorrections = 100
    let mutable converged = false
    let mutable result = nan

    while not converged && i < maxiter do
        let mutable xnext = g x
        let mutable corr = 0

        while (Double.IsInfinity(xnext) || Double.IsNaN(xnext)) && corr < maxcorrections do
            x <- x + eps
            xnext <- g x
            corr <- corr + 1

        if corr >= maxcorrections then
            result <- nan
            converged <- true
        else
            if abs(xnext - x) < eps && abs(f xnext) < eps then
                result <- xnext
                converged <- true
            else
                x <- xnext
                i <- i + 1

    if not converged then
        (nan, i)
    else
        (result, i)

let newtonmethod (x0: float) f f' =
    let mutable x = x0
    let mutable i = 0
    let mutable converged = false
    let mutable result = nan

    while not converged && i < maxiter do
        if Double.IsInfinity(x) || Double.IsNaN(x) then
            result <- nan
            converged <- true
        else
            let fx = f x
            let fpx = f' x

            if Double.IsInfinity(fpx) || Double.IsNaN(fpx) || abs fpx < 1e-15 then
                result <- nan
                converged <- true
            else
                let xnext = x - fx / fpx

                if Double.IsInfinity(xnext) || Double.IsNaN(xnext) then
                    result <- nan
                    converged <- true
                else
                    if abs (xnext - x) < eps && abs (f xnext) < eps then
                        result <- xnext
                        converged <- true
                    else
                        x <- xnext
                        i <- i + 1

    if not converged then
        (nan, i)
    else
        (result, i)

[<EntryPoint>]
let main argv =
    let results = [
        ("Дихотомия", "func1", "[1.0, 4.0]", bisectionmethod 1.0 4.0 func1)
        ("Дихотомия", "func2", "[-1.0, 15.0]", bisectionmethod -1.0 15.0 func2)
        ("Дихотомия", "func3", "[-1.0, 0.0]", bisectionmethod -1.0 0.0 func3)
        ("Итераций", "func1", "x0=1.5", iterationmethod 1.5 func1 g1)
        ("Итераций", "func2", "x0=3.0", iterationmethod 3.0 func2 g2)
        ("Итераций", "func3", "x0=0.1", iterationmethod 0.1 func3 g3)
        ("Ньютона", "func1", "x0=1.5", newtonmethod 1.5 func1 deriv1)
        ("Ньютона", "func2", "x0=3.0", newtonmethod 3.0 func2 deriv2)
        ("Ньютона", "func3", "x0=0.5", newtonmethod 0.5 func3 deriv3)
    ]

    printfn "| %10s | %8s | %15s | %20s | %8s |" "Метод" "Функция" "Параметры" "Корень" "Итерации"

    printfn "|%s|%s|%s|%s|%s|" (String.replicate 12 "-") (String.replicate 10 "-") (String.replicate 17 "-") (String.replicate 22 "-") (String.replicate 10 "-")

    for (methodName, funcName, paramsStr, (root, iter)) in results do
        let rootStr = if Double.IsNaN(root) then "не найден" else sprintf "%.10f" root
        let iterStr = if iter = -1 then "---" else sprintf "%d" iter
        printfn "| %10s | %8s | %15s | %20s | %8s |" methodName funcName paramsStr rootStr iterStr

    0