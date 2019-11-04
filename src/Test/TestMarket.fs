(**
Test markets using fscheck
========================
*)
module TestMarket

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open Commod
open Commod.ContractDates.Conventions

[<Property>]
let ``test getCalendar`` (d: Instrument) =
    let cal = getCalendar d 
    let b = Set.empty
    if d = JCC then cal = b
    else cal <> b

[<Property>]
let ``test getJkmPeriod`` () =
    (getJkmPeriod "Nov19" = (DateTime(2019,9,14), DateTime(2019,10,15))) .&.
    (getJkmPeriod "Jul20" = (DateTime(2020,5,16), DateTime(2020,6,15))) .&.
    (getJkmPeriod "Dec20" = (DateTime(2020,10,16), DateTime(2020,11,13)))

[<Property>]
let ``test getJccVolPeriod`` () =
    (getJccVolPeriod "Nov19" = (DateTime(2019,10,1), DateTime(2019,10,31))) .&.
    (getJccVolPeriod "Jul20" = (DateTime(2020,6,1), DateTime(2020,6,30))) .&.
    (getJccVolPeriod "Mar20" = (DateTime(2020,2,1), DateTime(2020,2,29)))


[<Property>]
let ``test getCommod `` (ins:Instrument) =
    let test = getCommod ins
    let (ContractDates ctt) = test.Contracts
    Assert.False ctt.IsEmpty |@ sprintf "Contracts are not empty" .&.
    Assert.True (test.Lot > 0M) |@ sprintf "Lot size is greater than 0"

// [<Property( MaxTest = 1)>]
// let ``test getPrices for BRT `` () =
//     let (PriceCurve p) = getPrices BRT
//     let s = p.Values |> Seq.filter( fun v -> v.Value < 0M)
//     Assert.Empty s  |@ "All prices are greater than 0"

[<Property( MaxTest = 100)>]
let ``test getPrices`` (ins:Instrument) =
    let (PriceCurve p) = getPrices ins
    let s = p |> Map.filter( fun _ v -> v.Value < 0M)
    Assert.Empty s  |@ "All prices are greater than 0"

[<Property( MaxTest = 100)>]
let ``test getVols`` () =
    let v = getVols JCC
    let s = 
        v.Pillars
        |> Set.toSeq
        |> Seq.map v.Item
        |> Seq.filter ( fun v -> v < 0M)
    Assert.Empty s |@ "All vols are greater than 0"