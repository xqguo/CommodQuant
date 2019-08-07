namespace Commod
module Markets = 
    open Calendars
    open Deedle

    //type PriceCurve<[<Measure>]'u> = PriceCurve of Series<string, float<'u>> //prices with quotation
    // these depends on the data format, as in PriceVols.csv
    let getPrices (df:Frame<string,string>) ins = 
        let (ContractDates c ) = getContracts ins
        let tag = ins.ToString()
        df?(tag)
        |> Series.filter( fun k _ -> c.ContainsKey k)

    let getCommod q lotsize ins = 
        let cals = getCalendar ins calendars
        let contracts = getContracts ins    
        { Calendar = cals; Contracts = contracts; Quotation = q; LotSize = lotsize}  


    let applyUSDbbl s = s |> Series.mapValues (fun v -> v * 1.<USD/bbl>)
    let applyUSDmmbtu s = s |> Series.mapValues (fun v -> v * 1.<USD/mmbtu>)
    let inline removeMeasure s = s |> Series.mapValues float

    let getBrtPrices df = getPrices df BRT |> applyUSDbbl
    let getJccPrices df = getPrices df JCC |> applyUSDbbl
    let getJkmPrices df = getPrices df JKM |> applyUSDmmbtu
    let getTtfPrices df = getPrices df TTF |> applyUSDmmbtu

    ///assuming raw data unit is in %. 
    let getVols (df:Frame<string,string>) ins = 
        let (ContractDates c ) = getContracts ins
        let tag = ins.ToString() + " Vol"
        df?(tag)
        |> Series.filter( fun k _ -> c.ContainsKey k)
        |> Series.mapValues( fun v -> v /100.)

    let filterCurve p pillars = 
        p |> Series.filter( fun k _ -> Set.contains k pillars)

    let shiftCurve p (v:seq<float>) = 
        ((Series.keys p), p.Values, v ) |||> Seq.map3( fun p v0 v1 -> (p, (v0 + v1 ))) |> series

    let inline overrideCurve p (v:seq<float>) = 
        ((Series.keys p), p.Values, v ) |||> Seq.map3( fun p v0 v1 -> (p, ((v0 + 1.0<_> - v0 )* v1 ))) |> series

    //these depends on the data format, as in PriceFixes.csv
    //let getFixes (df:Frame<DateTime,string>) ins = 
    //     let tag = ins.ToString()
    //     df?(tag)
    // let getBrtFixes df = getFixes df BRT |> applyUSDbbl
    // let getJccFixes df = getFixes df JCC |> applyUSDbbl
    // let getJkmFixes df = getFixes df JKM |> applyUSDmmbtu