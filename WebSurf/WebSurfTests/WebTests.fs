module WebSurfTests

open NUnit.Framework
open FsUnit
open WebSurf.WebSurf
[<Test>]
let ``simple test to check that function downloads links`` () =
    let result = getAllLinks "https://fsharpforfunandprofit.com/"
    result.Length |> should equal 2

[<Test>]
let ``another test`` () =
    let result = getAllLinks "https://www.google.com/"
    result.Length |> should greaterThan 0


[<Test>]
let ``test with wrong site`` () =
    let result = getAllLinks "https://nonexistent_site"
    result.IsEmpty |> should be True