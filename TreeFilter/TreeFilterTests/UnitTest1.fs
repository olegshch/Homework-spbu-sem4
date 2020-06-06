module TreeFilterTests

open NUnit.Framework
open TreeFilter.Tree
open FsUnit

[<Test>]
let ``Simple Tree`` () =
    treeFilter (Node(1, Emmpty, Emmpty)) (fun x -> x > 0) |> should equal [1]
