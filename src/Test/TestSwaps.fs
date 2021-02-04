module TestSwaps
open System
open Xunit
open FsCheck
open FsCheck.Xunit
open Commod

[<Property(MaxTest=3)>]
let ``test futPricing`` (ins:Instrument) =
    //let ins = BRT
    let cmd = getCommod ins
    let p = getPrices ins
    let ( PriceCurve c ) = p
    let mv = 
        c 
        |> Map.toList
        |> List.map(  fun (m, k) -> 
            let fut = 
                { Fut = cmd; ContractMonth = m; Quantity = 1M<lot>; FixedPrice = k }
            let v = genericFuturePricer fut p
            v.Value )
    let exp = List.replicate c.Count 0M
    Assert.False( c.IsEmpty )  |@ "Curve is no empty" .&.
    Assert.Equal<decimal list>( mv, exp)  |@ "All Fut at matrket price have mtk value"

//TODO add nrby and swap tests
[<Property(MaxTest=1)>]
let ``test nrby`` () =
    let ins = BRT
    // let (PositiveInt i ) = i    
    let cmd = getCommod ins
    let crv = getPrices ins
    let allPillars = crv.Pillars
    let avg = 
        {
           Commod = cmd
           Frequency =  AverageFrequency.BusinessDays
           RollAdj = 0
           Nrby = 0  
        }
    let cnt = getNrbyContracts avg
    let avg1 = 
        {
            avg with Nrby = 1
        }
    let cnt1 = getNrbyContracts avg1
    //limit d to be between today and curve end 
    let dmin = DateTime.Today
    let alldates = cnt.Fut
    let opendates = alldates |> Map.toArray |> Array.choose( fun (c,x) -> if x >= dmin && allPillars.Contains c then Some x else None ) |> Array.sort
    let dates0 = opendates.[0 .. (opendates.Length - 2)] 
    let dates1 =  opendates.[1 .. (opendates.Length - 1)] 
    let p = getFixingPrices cnt dates1 crv //nrby 0 on next month
    let p' = getFixingPrices cnt1 dates0 crv //nrby 1 on current month
    Assert.Equal<seq<UnitPrice>>( p, p')  |@ "nrby 1 will shift price pillar out by 1 month"

[<Property(MaxTest=1)>]
let ``test rolladjust`` () =
    let ins = BRT
    // let (PositiveInt i ) = i    
    let cmd = getCommod ins
    let crv = getPrices ins
    let allPillars = crv.Pillars
    let avg = 
        {
           Commod = cmd
           Frequency =  AverageFrequency.BusinessDays
           RollAdj = 0
           Nrby = 0  
        }
    let cnt = getNrbyContracts avg
    let avg1 = 
        {
            avg with RollAdj = 1
        }
    let cnt1 = getNrbyContracts avg1
    //limit d to be between today and curve end 
    let dmin = DateTime.Today
    let alldates = cnt.Fut
    let opendates = alldates |> Map.toArray |> Array.choose( fun (c,x) -> if x >= dmin && allPillars.Contains c then Some x else None ) |> Array.sort
    let dates0 = opendates.[0 .. (opendates.Length - 2)] 
    let dates1 =  opendates.[1 .. (opendates.Length - 1)] 
    let p = getFixingPrices cnt dates1 crv //nrby 0 on next month
    let p' = getFixingPrices cnt1 dates0 crv //nrby 1 on current month
    Assert.Equal<seq<UnitPrice>>( p, p')  |@ "rolladj 1 will shift price pillar out by 1 month"