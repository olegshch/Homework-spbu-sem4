module TestingMyQueue

open NUnit.Framework
open FsUnit
open System

[<Test>]
let ``Success Enque`` () =
    let queue = MyQueue.MyQueue()
    queue.Enqueue(5)
    queue.Dequeue |> should equal 5

[<Test>]
let ``Success several Enques-Decues`` () =
    let queue = MyQueue.MyQueue()
    queue.Enqueue(5)
    queue.Enqueue(10)
    queue.Enqueue(3)
    queue.Dequeue |> should equal 5
    queue.Dequeue |> should equal 10

[<Test>]
let ``Fails with empty Deque`` () =
    let queue = MyQueue.MyQueue()
    (fun() -> queue.Dequeue |> ignore) |> should throw typeof<InvalidOperationException>
