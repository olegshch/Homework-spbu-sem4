module FsLazyTests

open NUnit.Framework
open FsUnit
open System.Threading
open FsLazy.Lazy

[<Test>]
let ``simple test to check that single lazy works`` () =
    let singleLazy = LazyFactory.CreateSimpleLazy(fun () -> 34)
    singleLazy.Get() |> should equal 34

[<Test>]
let ``test to check that single lazy returns none with none supplier`` () =
    let singleLazy = LazyFactory.CreateSimpleLazy(fun () -> None)
    singleLazy.Get() |> should equal None

[<Test>]
let ``test to check that single lazy was calculated once`` () =
    let mutable current = 66
    let singleLazy = LazyFactory.CreateSimpleLazy(fun () -> Interlocked.Increment(ref current))
    let firstResult = singleLazy.Get()
    let secondResult = singleLazy.Get()
    firstResult |> should equal secondResult

[<Test>]
let ``simple test to check that multithreaded lazy works`` () =
    let multiLazy = LazyFactory.CreateConcurrentLazy(fun () -> 50)
    multiLazy.Get() |> should equal 50

[<Test>]
let ``test to check that multithreaded lazy returns none with none supplier`` () =
    let multiLazy = LazyFactory.CreateConcurrentLazy(fun () -> None)
    multiLazy.Get() |> should equal None

[<Test>]
let ``test to check that multithreaded lazy calculated once`` () =
    let mutable current = 8
    let multiLazy = LazyFactory.CreateConcurrentLazy(fun () -> Interlocked.Increment(ref current))
    let firstResult = 9
    let supplier () = multiLazy.Get() |> should equal firstResult
    let threads = Array.init 10000 (fun _ -> Thread(supplier))
    for thread in threads do
        thread.Start()
    for thread in threads do
        thread.Join()

[<Test>]
let ``simple test to check that lock-free lazy works`` () =
    let lockFreeLazy = LazyFactory.CreateLockFreeLazy(fun () -> 2)
    lockFreeLazy.Get() |> should equal 2

[<Test>]
let ``test to check that lock-free lazy returns none with none supplier`` () =
    let lockFreeLazy = LazyFactory.CreateLockFreeLazy(fun () -> None)
    lockFreeLazy.Get() |> should equal None

[<Test>]
let ``test to check that lock-free lazy calculated once`` () =
    let mutable current = 7
    let lockFreeLazy = LazyFactory.CreateLockFreeLazy(fun () -> Interlocked.Increment(ref current))
    let firstResult = 8
    let supplier () = lockFreeLazy.Get() |> should equal firstResult
    let threads = Array.init 10000 (fun _ -> Thread(supplier))
    for thread in threads do
        thread.Start() 
    for thread in threads do
        thread.Join()