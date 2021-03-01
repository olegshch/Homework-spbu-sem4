namespace Workflows

open System

module Rounder =

    /// Округление чисел
    type Rounder(precision: int) =
        member this.Bind(x: float, f) =
            f <| Math.Round(x, precision)
        member this.Return(x: float) =
            Math.Round(x, precision)

module StringConverter = 

    /// Обработка "строковых" чисел
    type StringNumbers() =
        member this.Bind(x: string, f) =
            match Int32.TryParse(x) with
            | true, number -> f <| number
            | _ -> None
        member this.Return(x) =
            Some(x)