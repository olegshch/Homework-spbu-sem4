module LocalNetVirusTests

open NUnit.Framework
open FsUnit
open LocalNetVirus.Models
open System

let invurnelableOS () = OS(0.0)

let overvulnerableOS () = OS(1.0)

let overvulnerableComputers () =
    [
        [
        Computer(Guid.NewGuid(), overvulnerableOS(), true)
        Computer(Guid.NewGuid(), overvulnerableOS(), false)
        Computer(Guid.NewGuid(), overvulnerableOS(), false)
        Computer(Guid.NewGuid(), overvulnerableOS(), false)
        Computer(Guid.NewGuid(), overvulnerableOS(), false)
        ]
    ] |> Seq.map (fun (computers) -> TestCaseData(computers))

let invulnerableComputers () =
    [
        [
            Computer(Guid.NewGuid(), invurnelableOS(), true)
            Computer(Guid.NewGuid(), invurnelableOS(), false)
            Computer(Guid.NewGuid(), invurnelableOS(), false)
        ], 1
    ] |> Seq.map (fun (computers, infected) -> TestCaseData(computers, infected))

[<TestCaseSource("overvulnerableComputers")>]
let ``If probability is 1 infection should behave like bfs`` (computers) =
    let matrix =
        [
            [false; true; false; true; true]
            [true; false; true; false; true]
            [false; true; false; true; false]
            [true; false; true; false; true]
            [true; true; false; true; false]
        ]

    let net = Network(computers, matrix)
    net.Computers |> List.map(fun x -> x.IsInfected) |> should equal [true; false; false; false; false]
    net.Infect() |> should equal true
    net.Computers |> List.map(fun x -> x.IsInfected) |> should equal [true; true; false; true; true]
    net.Infect() |> should equal true
    net.Computers |> List.map(fun x -> x.IsInfected) |> should equal [true; true; true; true; true]
    net.Infect() |> should equal false

[<TestCaseSource("invulnerableComputers")>]
let ``If probability is 0 infection should not happen`` (computers, infected) =
    let matrix =
        [
            [false; true; true]
            [true; false; false]
            [true; false; false]
        ]

    let net = Network(computers, matrix)
    net.Infect() |> should equal false

[<Test>]
let ``No infections with clear net`` () =
    let computers = [Computer(Guid.NewGuid(), OS(0.5), false); Computer(Guid.NewGuid(), OS(0.5), false)]

    let matrix =
        [
            [false; true]
            [true; false]
        ]

    let net = Network(computers, matrix)
    net.Infect() |> should equal false

[<TestCaseSource("overvulnerableComputers")>]
let ``No infections with isolated infection`` (computers) =
    let matrix =
        [
            [false; false; false; false; false]
            [false; false; true; false; true]
            [false; true; false; true; false]
            [false; false; true; false; true]
            [false; true; false; true; false]
        ]

    let net = Network(computers, matrix)
    net.Infect() |> should equal false
