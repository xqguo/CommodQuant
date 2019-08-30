module TestSwaps

open Xunit
open FsCheck
open FsCheck.Xunit
open Commod
open Deedle

[<Property>]
let ``test futPricing`` (ins:Instrument) =
    //let ins = BRT
    let cmd = getCommod ins
    let p = getPrices ins
    let ( PriceCurve c ) = p
    let mv = 
        c 
        |> Series.observations
        |> Seq.toList
        |> List.map(  fun (m, k) -> 
            let fut = 
                { Fut = cmd; ContractMonth = m; Quantity = 1M<lot>; FixedPrice = k }
            let v = genericFuturePricer fut p
            v.Value )
    let exp = List.replicate c.KeyCount 0M
    Assert.False( c.IsEmpty )  |@ "Curve is no empty" .&.
    Assert.Equal<decimal list>( mv, exp)  |@ "All Fut at matrket price have mtk value"
