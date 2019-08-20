namespace Commod
module Markets = 
    open IOcsv
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

    ///appy a function on decimals to a q1 quantity amount for a commod with conversion to q2 first.
    let mapQuantity f (q1:QuantityAmount) (q2:QuantityAmount) (c:Commod)=
        let qty2 = q2 |> getCaseDecimal |> fst
        let x = qtyConversion q1 qty2 c |> getCaseDecimal |> snd
        f x |>  QuantityAmount.applyCase qty2

    let lotsConversion (q:QuantityAmount) (i:Commod) =            
        let getLots x = x / i.Lot
        mapQuantity getLots q i.LotSize i

    let inline applySeriesUnit case s = s |> Series.mapValues ( UnitPrice.applyCase case )

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

    // these depends on the data format, as in BRT ICE_Price.csv
    let getPrices ins = 
        let i = getCommod ins
        let (ContractDates c) = i.Contracts
        let f = tryPriceFile ins
        match f with 
        | Some v -> 
            PriceCsv.Load(v).Rows
            |> Seq.filter( fun r -> c.ContainsKey r.PILLAR)
            |> Seq.map( fun r ->  r.PILLAR, r.PRICE)
            |> series
            |> applySeriesUnit i.Quotation.Case
            |> PriceCurve
        | None -> invalidOp <| sprintf "Cannot load prices for %A" ins

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
