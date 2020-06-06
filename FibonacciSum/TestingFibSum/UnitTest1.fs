module TestingFibSum

open NUnit.Framework

open FsUnit

[<Test>]
let ``True Summ`` () =
    FibonacciSum.FibSum.counting |> should equal 1089154
