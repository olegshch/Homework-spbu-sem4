module TestsForFunctions

open NUnit.Framework
open FsUnit
open SimpleFunctions.Functions
[<Test>]
let ``Factorial test`` () =
    factorial 5 |> should equal 120

[<Test>]
let ``Fibonacci test`` () =
    fibonacci 10 |> should equal 55

[<Test>]
let ``Reverse test`` () =
    reverseList [1; 2; 3; 4; 5] |> should equal [5; 4; 3; 2; 1]

[<Test>]
let ``Power test`` () =
    powerTwoList 4 7 |> should equal [16; 32; 64; 128; 256; 512; 1024; 2048]

[<Test>]
let ``Find test`` () =
    find [3; 4; 5] 4 |> should equal 1