module TestUnitTypes

open FsCheck.FSharp
open FsCheck.Xunit
open Commod

[<Property>]
let ``test getCaseDecimal and applyCase for QuantityAmount`` (a: QuantityAmount) =
    let c, v = a |> getCaseDecimal
    let b = QuantityAmount.applyCase c v
    (a = b)
    |> Prop.label (sprintf "input: %A, output: %A" a b)
