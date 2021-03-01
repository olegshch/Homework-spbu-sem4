module PointFreeTests

open NUnit.Framework
open FsCheck
open Point_Free.PointFree

[<Test>]
let ``Must be equal results`` () = 
    Check.QuickThrowOnFailure (fun x l ->
    mult1 x l = mult2 x l && 
    mult2 x l = mult3 x l && 
    mult3 x l = multPointFree x l)
