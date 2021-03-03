module LambdaInterpretatorTesting

open NUnit.Framework
open FsUnit
open LambdaInterpretator.LambdaInterpretator

let testCases =
    [
        App(Abs('x', Var('x')), Var('y')), Var('y')
        Var('x'), Var('x')
        App(Abs('x', Var('x')), Var('y')), Var('y')
        App(Var('x'), Var('y')), App(Var('x'), Var('y'))
        Abs('x', App(Abs('y', Var('y')), Var('x'))), Abs('x', Var('x'))
    ] |> List.map (fun (exp, res) -> TestCaseData(exp, res))

[<Test>]
[<TestCaseSource("testCases")>]
let ``Beta reduction tests`` exp res =
    reduction exp |> should equal res

