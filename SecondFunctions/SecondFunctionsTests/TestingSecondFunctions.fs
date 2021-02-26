module SecondFunctionsTests

open NUnit.Framework
open SecondFunctions
open FsUnit

let testCasesSequenceOfPrimeNumbers =
    [
        0, 2
        1, 3
        2, 5
        3, 7
        9, 29
        15, 53
        24, 97
        30, 127
        49, 229
    ] |> List.map (fun (n, res) -> TestCaseData(n, res))

[<Test>]
[<TestCaseSource("testCasesSequenceOfPrimeNumbers")>]
let sequenceOfPrimeNumbersTest n res =
    SecondFunctions.PrimeGeneration.primeSequence () |> Seq.item n |> should equal res
