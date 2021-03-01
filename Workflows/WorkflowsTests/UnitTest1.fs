module WorkflowsTests

open NUnit.Framework
open FsUnit
open Workflows.Rounder
open Workflows.StringConverter

let stringNumbers = new StringNumbers()

[<Test>]
let ``SimpleRounderTest`` () =
    let rounder = new Rounder(3)
    rounder {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    } |> should (equalWithin 0.0001) 0.048

[<Test>]
let ``SecondSimpleRounderTest`` () =
    let rounder = new Rounder(2)
    rounder {
        let! a = 3.0 / 11.0
        let! b = 5.5
        return a / b
    } |> should (equalWithin 0.001) 0.05

[<Test>]
let ``ComplexRounderTest`` () =
    let rounder = new Rounder(4)
    rounder {
        let! a = 2.0 / 13.0
        let! b = 3.58 * 0.147
        let! c = 13.0 / 19.0
        let! d = b / c
        return a / d
    } |> should (equalWithin 0.00001) 0.1999

[<Test>]
let ``AnotherComplexRounderTest`` () =
    let rounder = new Rounder(3)
    rounder {
        let! a = 2.0 / 19.0
        let! b = 0.35 * 0.98421
        let! c = a * b
        let! d = 0.963 * 2.6137
        return d / c
    } |> should (equalWithin 0.0001) 69.917

[<Test>]
let ``SimpleSomeStringNumbersTest`` () =
    stringNumbers {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    } |> should equal (Some(3))

[<Test>]
let ``SimpleNoneStringNumbersTest`` () =
    stringNumbers {
        let! x = "1"
        let! y = "a"
        let z = x + y
        return z
    } |> should equal (None)

[<Test>]
let ``ComplexSomeStringNumbersTest`` () =
    stringNumbers {
        let! a = "1"
        let! b = "2"
        let! c = "147"
        let d = a + b + c
        let! e = "30"
        return d / e
    } |> should equal (Some(5))

[<Test>]
let ``ComplexNoneStringNumbersTest`` () =
    stringNumbers {
        let! a = "1"
        let! b = "2"
        let c = a + b
        let! d = "O"
        let e = c + d
        return e
    } |> should equal (None)
