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
open Commod.Contracts.Conventions

[<Property>]
let ``test getCalendar`` (d: Instrument) =
    let cal = getCalendar d 
    match d with
    | JCC -> cal = Set.empty
    | _ -> true

[<Property( MaxTest = 1)>]
let ``test brtContractRule`` () =
    let c = getCommod BRT 
    let cnt = c.Contracts.Fut
    let r = 
        Map.filter( fun k v -> v <> (pillarToDate k  |>  getBrtExp) ) cnt
        |> Map.map(  fun k v -> v , (pillarToDate k  |>  getBrtExp) ) 
    //r |> Map.count < 4 //3 known diffs
    Map.isEmpty r

[<Property( MaxTest = 1)>]
let ``test ttfContractRule`` () =
    let c = getCommod TTF 
    let cnt = c.Contracts.Fut
    let r = 
        Map.filter( fun k v -> v <> (pillarToDate k  |>  getTtfExp) ) cnt
        |> Map.map(  fun k v -> v , (pillarToDate k  |>  getTtfExp) ) 
    //r |> Map.count < 2 //1 known diffs
    Map.isEmpty r

[<Property( MaxTest = 1)>]
let ``test ttfOptContractRule`` () =
    let c = getCommod TTF 
    let cnt = c.Contracts.Opt
    let r = 
        Map.filter( fun k v -> v <> (pillarToDate k  |>  getTtfOptExp) ) cnt
        |> Map.map(  fun k v -> v , (pillarToDate k  |>  getTtfOptExp) ) 
    //r |> Map.count < 2 //1 known diffs
    Map.isEmpty r

[<Property( MaxTest = 1)>]
let ``test brtOptContractRule`` () =
    let c = getCommod BRT 
    let cnt = c.Contracts.Opt
    let r = 
        Map.filter( fun k v -> v <> (pillarToDate k  |>  getBrtOptExp) ) cnt
        |> Map.map(  fun k v -> v , (pillarToDate k  |>  getBrtOptExp) ) 
    //r |> Map.count < 2 //1 known diffs
    Map.isEmpty r


//[<Property>]
//let ``test getJkmPeriod`` () =
//    (getJkmPeriod "Nov19" = (DateTime(2019,9,14), DateTime(2019,10,15))) .&.
//    (getJkmPeriod "Jul20" = (DateTime(2020,5,16), DateTime(2020,6,15))) .&.
//    (getJkmPeriod "Dec20" = (DateTime(2020,10,16), DateTime(2020,11,13)))

//[<Property>]
//let ``test getJccVolPeriod`` () =
//    (getJccVolPeriod "Nov19" = (DateTime(2019,10,1), DateTime(2019,10,31))) .&.
//    (getJccVolPeriod "Jul20" = (DateTime(2020,6,1), DateTime(2020,6,30))) .&.
//    (getJccVolPeriod "Mar20" = (DateTime(2020,2,1), DateTime(2020,2,29)))


[<Property>]
let ``test getCommod `` (ins:Instrument) =
    let test = getCommod ins
    let (ContractDates ctt) = test.Contracts
    Assert.False ctt.IsEmpty |@ sprintf "Contracts are not empty" .&.
    Assert.True (test.Lot > 0M) |@ sprintf "Lot size is greater than 0" .&.
    Assert.True (test.Instrument = ins) |@ sprintf "Instrument is the same."

// [<Property( MaxTest = 1)>]
// let ``test getPrices for BRT `` () =
//     let (PriceCurve p) = getPrices BRT
//     let s = p.Values |> Seq.filter( fun v -> v.Value < 0M)
//     Assert.Empty s  |@ "All prices are greater than 0"

[<Property( MaxTest = 5)>]
let ``test getPrices`` (ins:Instrument) =
    let (PriceCurve p) = getPrices ins
    let s = p |> Map.filter( fun _ v -> v.Value < 0M)
    Assert.Empty s |@ "All prices are greater than 0" 

[<Property( MaxTest = 1)>]
let ``test getVols`` () =
    let v = getVols JCC
    let s = 
        v.Pillars
        |> Set.toSeq
        |> Seq.map v.Item
        |> Seq.filter ( fun v -> v < 0M)
    Assert.Empty s |@ "All vols are greater than 0"