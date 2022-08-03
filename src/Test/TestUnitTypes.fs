module TestUnitTypes

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open FsCheckTypes
open Commod


[<Property>]
let ``test getCaseDecimal and applyCase for QuantityAmount`` (a:QuantityAmount) =     
    let c,v = a |> getCaseDecimal
    let b = QuantityAmount.applyCase c v
    a = b |@ "Reconstructed Quantity Amount is the same as input" 

    

