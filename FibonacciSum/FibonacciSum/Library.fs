namespace FibonacciSum

module FibSum =
    let Counting =
        let rec Summ i s =
            let Fibonacci n =
                let rec Fib a b i n =
                    if i = n then a
                    else Fib b (a + b) (i + 1) n
                Fib 1 1 1 i
            if (Fibonacci i) > 1000000 then s
            elif (Fibonacci i) % 2 = 0 then Summ (i + 1) (s + Fibonacci i)
            else Summ (i + 1) s
        Summ 1 0