namespace Commod
module Markets = 
    open Calendars
    open Deedle
    open FSharp.Reflection

    let inline applySeriesUnit case s = s |> Series.mapValues ( UnitPrice.applyCase case )
    ///f is the starting quantity to be converted into the unit of t. 
    ///amount of t is ignored.
    let unitConversion (f:QuantityAmount) (t:QuantityAmount) (i:Commod) = 
        let bblmtFO = 6.35M<bbl/mt> 
        match (f,t ) with
        | BBL _, BBL _ -> f
        | MT _, MT _ -> f
        | _ -> 
            match i.Instrument with
            | FO180 | FO380 ->             
                match (f,t ) with
                | MT f, BBL _ -> f * bblmtFO |> BBL
                | BBL f, MT _ -> f / bblmtFO |> MT
                | _ -> invalidOp "Unit conversion not implemented"
            | _ -> invalidOp "Not implemented"

    ///appy a function on decimals to a q1 quantity amount for a commod with conversion to q2 first.
    let mapQuantity f (q1:QuantityAmount) (q2:QuantityAmount) (c:Commod)=
        let c,x = unitConversion q1 q2 c |> getCaseDecimal
        f x |>  QuantityAmount.applyCase c

    let lotsConversion (q:QuantityAmount) (i:Commod) =            
        let getLots x = x / i.Lot
        mapQuantity getLots q i.LotSize i


    /////appy a function for decimal to a q1 quantity amount for a commod with conversion to q2 first.
    /////appy a function for decimal to a q1 quantity amount for a commod with conversion to q2 first.
    //let applyUnitPrice f (p1:UnitPrice) (p2:UnitPrice) (c:Commod)=
    //    let t1 = Uni
    //    let q = unitConversion q1 q2 c   
    //    match q. with
    //    | BBL x -> decimal x |> f |> applyBBL
    //    | MMBTU x -> decimal x |> f |> applyMMBTU
    //    | MT x -> decimal x |> f |> applyMT

    //type PriceCurve<[<Measure>]'u> = PriceCurve of Series<string, float<'u>> //prices with quotation
    // these depends on the data format, as in BRT ICE_Price.csv
    let getPrices ins = 
        let (ContractDates c ) = getContracts ins
        let (f,applyMeasure) = 
            match ins with
            | BRT -> "BRT ICE_price.csv", (applySeriesUnit "USDBBL")
            | _ -> invalidOp "Not implemented"
        PriceCsv.Load(f).Rows
        |> Seq.filter( fun r -> c.ContainsKey r.PILLAR)
        |> Seq.map( fun r ->  r.PILLAR, r.PRICE)
        |> series
        |> applyMeasure

    let getCommod ins = 
        let getCommod' q lotsize ins = 
            let cals = getCalendar ins calendars
            let contracts = getContracts ins    
            { Instrument = ins; Calendar = cals; Contracts = contracts; Quotation = q; LotSize = lotsize}  
        let q = 
            match ins with
            | BRT | JCC -> USDBBL 1M<USD/bbl> 
            | JKM | TTF -> USDMMBTU 1M<USD/mmbtu>
        let s = 
            match ins with
            | BRT | JCC -> BBL 1000M<bbl>
            | JKM | TTF -> MMBTU 10000M<mmbtu>
        getCommod' q s ins

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
