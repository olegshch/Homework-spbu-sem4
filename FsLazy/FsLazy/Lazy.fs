namespace FsLazy

open System
open System.Threading

module Lazy =

    /// Lazy интерфейс
    type ILazy<'a> =

        /// Получение результата
        abstract member Get: unit -> 'a

    /// Простая версия 
    type SimpleLazy<'a>(supplier: unit -> 'a) =
        let mutable supplier = Some(supplier)
        let mutable result = None
        interface ILazy<'a> with
            member this.Get() =
                if (result.IsNone) then
                    result <- Some(supplier.Value())
                    supplier <- None
                result.Value

    /// Многопоточная версия
    type ConcurrentLazy<'a>(supplier: unit -> 'a) =
        let locker = obj()
        let mutable supplier = Some(supplier)
        let mutable result = None
        interface ILazy<'a> with
            member this.Get() =
                match result with
                | Some value -> value 
                | None -> lock locker (fun () ->
                    match result with
                    | Some value -> value
                    | None ->
                        result <- Some(supplier.Value())
                        supplier <- None
                        result.Value)

    /// Lock-free версия
    type LockFreeLazy<'a>(supplier: unit -> 'a) =
        let mutable result = None        
        interface ILazy<'a> with
            member this.Get () =
                let desiredVal = Some (supplier())
                Interlocked.CompareExchange(&result, desiredVal, None) |> ignore
                result.Value

    /// Lazy factory
    type LazyFactory<'a>() =

        static member CreateSimpleLazy(supplier: unit -> 'a) =
            SimpleLazy supplier :> ILazy<'a>

        static member CreateConcurrentLazy(supplier: unit -> 'a) =
            ConcurrentLazy supplier :> ILazy<'a>

        static member CreateLockFreeLazy(supplier: unit -> 'a) =
            LockFreeLazy supplier :> ILazy<'a>