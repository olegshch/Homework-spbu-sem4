module TestsForFunctions

open NUnit.Framework
open FsUnit

[<Test>]
let ``Factorial test`` () =
    SimpleFunctions.Functions.Factorial 5 |> should equal 120

[<Test>]
let ``Fibonacci test`` () =
    SimpleFunctions.Functions.Fibonacci 10 |> should equal 55

[<Test>]
let ``Reverse test`` () =
    SimpleFunctions.Functions.ReverseList [1;2;3;4;5] |> should equal [5;4;3;2;1]

[<Test>]
let ``Power test`` () =
    SimpleFunctions.Functions.PowerTwo 4 7 |> should equal [16; 32; 64; 128; 256; 512; 1024; 2048]

[<Test>]
let ``Find test`` () =
    SimpleFunctions.Functions.Find [3; 4; 5] 4 |> should equal 1