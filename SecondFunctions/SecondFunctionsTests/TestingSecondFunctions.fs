module SecondFunctionsTests

open NUnit.Framework
open SecondFunctions
open FsUnit
open ExpressionEvaluation
open MapTree

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
    PrimeGeneration.primeSequence () |> Seq.item n |> should equal res

[<Test>]
let ``TestingEvenCount`` () =
    EvenCount.evenFilter [1; 2; 3; 4; 5] |> should equal 2
    EvenCount.evenFold [1; 2; 3; 4; 5] |> should equal 2
    EvenCount.evenMap [1; 2; 3; 4; 5] |> should equal 2

[<Test>]
let ``TestingEvenCountInEmpty`` () =
    EvenCount.evenFilter [] |> should equal 0
    EvenCount.evenFold [] |> should equal 0
    EvenCount.evenMap [] |> should equal 0

[<Test>]
let ``Evaluating tree with operand`` () =
     eval (Operand(2)) |> should equal 2

[<Test>]
let ``Evaluating tree with sum`` () =
     eval (Sum(Operand(2), Operand(3))) |> should equal 5

[<Test>]
let ``Evaluating tree with difference`` () =
     eval (Diff(Operand(2), Operand(3))) |> should equal -1

[<Test>]
let ``Evaluating tree with multiply`` () =
    eval (Mult(Operand(2), Operand(3))) |> should equal 6

[<Test>]
let ``Evaluating tree with divide`` () =
    eval (Div(Operand(3), Operand(2))) |> should equal 1

[<Test>]
let ``Evaluating tree with zero divide`` () =
    (fun () -> eval (Div(Operand(2), Operand(0))) |> ignore) |> should throw typeof<System.DivideByZeroException>

[<Test>]
let ``MapTree test`` () =
    mapTree (fun x -> x + 1) (Node(2, Empty, Empty)) |> should equal (Node(3, Empty, Empty))

[<Test>]
let ``BiggerMapTree test`` () =
    mapTree (fun x -> x * -x) (Node(2, Node(5, Empty, Empty), Node(3, Empty, Node(0, Empty, Empty)))) |> should equal (Node(-4, Node(-25, Empty, Empty), Node(-9, Empty, Node(0, Empty, Empty))))
