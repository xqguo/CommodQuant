namespace Commod
module Rates = 
    open System
    open Utils
    open QLNet

    ///create USD OIS curve csv file with ref date
    ///Fed funds in ACT/360: https://www.federalreserve.gov/releases/h15/
    ///expected csv format:
    ///    PILLAR,PRICE
    ///    1M,2.3517
    ///    2M,2.3208
    let getUSDOIS (td:DateTime) = 
        let f = ( ROOT +/ "csv" +/ "USD OIS_Rate.csv" )
        let i = FedFunds()
        let helpers = 
            getPrice f
            |> Seq.map( fun (p,v) ->     
                match p with
                | Period (n, p ) ->
                    let u = 
                      match p with
                      |"W" -> TimeUnit.Weeks
                      |"M" -> TimeUnit.Months
                      |"Y" -> TimeUnit.Years
                      | _ -> invalidOp <| sprintf "Unknown pillar format, expect W/M/Y, got %s" p
                    let q = SimpleQuote(Nullable(v)) :> Quote
                    let h = Handle(q)
                    OISRateHelper(2, Period(n, u), h, i ) :> RateHelper        
                | _ -> invalidOp <| sprintf "Unknown pillar format, expect W/M/Y, got %s" p)
        let instruments = ResizeArray(helpers)
        PiecewiseYieldCurve<Discount, LogLinear>( Date td, instruments, Actual360())


