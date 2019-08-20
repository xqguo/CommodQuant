namespace Commod
module Markets = 
    open Calendars
    open Deedle

    let conversionFactors = 
        [ 
            //ins, from, to 
            (FO180, "MT", "BBL"), 6.35M 
            (FO380, "MT", "BBL"), 6.35M 
        ]
        |> dict

    let getConversion i qty qty2 = 
        let key = (i,qty,qty2) 
        if conversionFactors.ContainsKey key then
            conversionFactors.[key] 
        else             
            let key' = (i,qty2,qty) 
            if conversionFactors.ContainsKey key' then
                1M / conversionFactors.[key'] 
            else 
                invalidOp <| sprintf "Conversion factor to %s from %s not found for %A" qty2 qty i

    let qtyConversion (f:QuantityAmount) (qty2:string) (i:Commod) = 
        let qty, x = getCaseDecimal f
        if qty = qty2 then 
            f 
        else             
            let y = getConversion i.Instrument qty qty2 
            x * y |> QuantityAmount.applyCase qty2

    ///convert price to be consistent with quantity amount based on commodity conversion factors
    let convertUnitPrice (qty2:string) (c:Commod) (p:UnitPrice)= 
        let quote, x = getCaseDecimal p
        let ccy1 = quote.Substring(0,3)
        let qty1 = quote.Substring 3 
        if qty1 = qty2 then p //no need to convert
        else
            let q1 = QuantityAmount.applyCase qty1 1M 
            let r = qtyConversion q1 qty2 c |> getCaseDecimal |> snd
            x * r |> UnitPrice.applyCase (ccy1+qty2)

    /////get ccy amount with built-in unit conversion
    //let rec getPrice (p:UnitPrice) (q:QuantityAmount) (c:Commod)= 
    //    match (p, q) with
    //    | USDBBL p , BBL q -> p * q |> CurrencyAmount.USD
    //    | USDMT p, MT q -> p * q |> CurrencyAmount.USD
    //    | _ -> 
    //        match c.Instrument with 
    //        | FO180 ->                 
    //            match (p, q) with
    //            | USDBBL _ , MT _ -> 
    //                let q1 = QuantityAmount.applyCase "MT" 1M 
    //                let q2 = QuantityAmount.applyCase "BBL" 1M 
    //                let q = qtyConversion q1 q2 c
    //                getPrice p q c
    //            |_ -> invalidOp "not implemented"

    ///appy a function on decimals to a q1 quantity amount for a commod with conversion to q2 first.
    let mapQuantity f (q1:QuantityAmount) (q2:QuantityAmount) (c:Commod)=
        let qty2 = q2 |> getCaseDecimal |> fst
        let x = qtyConversion q1 qty2 c |> getCaseDecimal |> snd
        f x |>  QuantityAmount.applyCase qty2

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

    let inline applySeriesUnit case s = s |> Series.mapValues ( UnitPrice.applyCase case )
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
        let (q,s) = 
            match ins with
            | BRT |DBRT | JCC | DUB | SJET | SGO  -> USDBBL 1M<USD/bbl>, BBL 1000M<bbl>
            | FO180 | FO380 | FO3_5 | GO ->  USDMT 1M<USD/mt>, MT 1000M<mt>
            | NG | JKM | TTF -> USDMMBTU 1M<USD/mmbtu>, MMBTU 10000M<mmbtu>            
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

    let getCurveUnit (PriceCurve p) = p |> Series.firstValue |> getCaseDecimal |> fst            
