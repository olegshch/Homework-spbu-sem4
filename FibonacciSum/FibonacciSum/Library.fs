namespace FibonacciSum

 
module FibSum =

    //Finds summ of even fibonaccies under 1000000
    let Counting =
        let rec fibonaccisumm previous current sum =
            if current < 1000000 && current % 2 = 0 then fibonaccisumm current (previous + current) (sum + current)
            elif current < 1000000 then fibonaccisumm current (previous + current) sum
            else sum
        fibonaccisumm 1 1 0