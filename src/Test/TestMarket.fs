(**
Test markets using fscheck
========================
*)
module TestMarket

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

[<Property>]
//test option contract rules vs actual exchange dates
let ``test opt ContractRule`` ins =
    let c = getCommod ins 
    let cnt = c.Contracts.Opt
    let f d = getOptExp d ins
    let r = 
        Map.filter( fun k v -> v <> (pillarToDate k  |>  f) ) cnt
        |> Map.map(  fun k v -> v , (pillarToDate k  |>  f) ) 
    //r |> Map.count < 2 //1 known diffs
    Map.isEmpty r |@ sprintf "%A" r

[<Property>]
//test contract rules vs actual exchange dates
let ``test fut ContractRule`` () =
    let ins = NG
    let c = getCommod ins
    let cnt = c.Contracts.Fut
    let f d = getExp d ins
    let r = 
        Map.filter( fun k v -> v <> (pillarToDate k  |>  f) ) cnt
        |> Map.map(  fun k v -> v , (pillarToDate k  |>  f) ) 
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
    (not ctt.IsEmpty) |@ sprintf "Contracts are not empty" .&.
    (test.Lot > 0M) |@ sprintf "Lot size is greater than 0" .&.
    (test.Instrument = ins) |@ sprintf "Instrument is the same."

[<Property( MaxTest = 1)>]
let ``test getVols`` ins =
    (tryVolsFile ins).IsSome ==> lazy(
        let v = getVols ins
        let s = 
            v.Pillars
            |> Set.toSeq
            |> Seq.map v.Item
            |> Seq.filter ( fun v -> v < 0M)
        Seq.isEmpty s |@ "All vols are greater than 0")

[<Property( MaxTest = 5)>]
let ``test getPrices`` (ins:Instrument) =
    match (tryPriceFile ins) with
    | None -> lazy( getPrices ins ) |> Prop.throws<System.InvalidOperationException,_>
    | Some _ -> lazy(
        let (PriceCurve p) = getPrices ins
        let s = p |> Map.filter( fun _ v -> v.Value < 0M)
        Map.isEmpty s |@ "All prices are greater than 0" ) |> Prop.ofTestable
        