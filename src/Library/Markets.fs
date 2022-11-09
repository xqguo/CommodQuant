namespace Commod
[<AutoOpen>]
module Markets = 
    open IOcsv
    open System
    open FSharp.Data
    //cache the commods as it involves disk IO
    let commoddict = new System.Collections.Concurrent.ConcurrentDictionary<Instrument, Commod>()

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

    let inline applyMapUnit case s = s |> Map.map( fun k v -> UnitPrice.applyCase case v )

    let createCommod = 
        let getCommod' ins = 
            let (q,s) = 
                match ins with
                | BRT |DBRT | JCC | DUB | SJET | SGO  -> USDBBL 1M<USD/bbl>, BBL 1000M<bbl>
                | FO180 | FO380 | MFO | FO35 | GO ->  USDMT 1M<USD/mt>, MT 1000M<mt>
                | NG | JKM | NBP | TTF | SPP | _ -> USDMMBTU 1M<USD/mmbtu>, MMBTU 10000M<mmbtu>            
            let cals = getCalendar ins 
            let contracts = getContracts ins    
            { Instrument = ins; Calendar = cals; Contracts = contracts; Quotation = q; LotSize = s}  
        fun ins -> getCommod' ins
               
    let getCommod ins = commoddict.GetOrAdd(ins, createCommod) 

    // these depends on the data format
    // commod curve pillars are either MMM-yy or TODAY or BOM, all in upper case.
    let getPrices ins = 
        let i = getCommod ins
        let (ContractDates c) = i.Contracts
        let f = tryPriceFile ins
        match f with 
        | Some v -> 
            getPrice v 
            |> Seq.map( fun (p,x) ->  
                let pillar = 
                    match p with
                    | "TODAY" -> "TODAY"
                    | s when s.StartsWith "BOM" -> "BOM"
                    | x -> pillarToDate x |> formatPillar
                pillar, decimal x)
            |> Seq.filter( fun (p,_) -> c.ContainsKey p || p = "TODAY" || p = "BOM" )
            |> Map.ofSeq
            |> applyMapUnit i.Quotation.Case
            |> PriceCurve
        | None -> invalidOp <| sprintf "Cannot load prices for %A" ins

    // these depends on the data format
    // commod curve pillars are either MMM-yy or TODAY or BOM, all in upper case.
    // vols data in market quote ( percent ), e.g. 20.1
    let getVols ins = 
        let i = getCommod ins
        let (ContractDates c) = i.Contracts
        let f = tryVolsFile ins
        match f with 
        | Some v -> 
            //let data = PriceCsv.AsyncLoad v |> Async.RunSynchronously
            //data.Rows
            getPrice v 
            |> Seq.map( fun (p,v) ->  
                let pillar = 
                    match p with
                    | "TODAY" -> "TODAY"
                    | s when s.StartsWith "BOM" -> "BOM"
                    | x -> pillarToDate x |> formatPillar
                pillar, ( PercentVol (decimal v)))
            |> Seq.filter( fun (p,_) -> c.ContainsKey p || p = "TODAY" || p = "BOM" )
            |> Map.ofSeq
            |> VolCurve
        | None -> invalidOp <| sprintf "Cannot load prices for %A" ins

    // commod curve pillars are in MMM-yy
    // vols data in market quote ( percent ), e.g. 20.1
    let getSmileFromCSV v = 
        let getnum x =  x |> Array.skip 1 |> Array.map( fun s -> ( Double.Parse s ) / 100. ) 
        let data = CsvFile.AsyncLoad v |> Async.RunSynchronously
        let deltas = 
            match data.Headers with 
            | Some h -> h |> getnum
            | None -> failwith "wrong smile file format"
        let (pillars,vols) = 
            data.Rows
            |> Array.ofSeq
            |> Array.map( fun r ->  
                let p = r.Columns |> Array.head
                let v = r.Columns |> getnum
                p,v)
            |> Array.unzip
        let fp = pillars |> Array.map (pillarToDate >> formatPillar)
        new VolDeltaSmile(fp,deltas,vols)

    let getSmile ins = 
        let f = trySmileFile ins
        match f with 
        | Some v -> getSmileFromCSV v
        | None -> invalidOp <| sprintf "Cannot load smile for %A" ins

    type CurveOveride =
        | Flatten
        | Shift

    let getPriceCurve ins o = 
        let c = getPrices ins
        match o with 
        | Some (m,p) -> 
            match m with
            | Flatten -> c.flatten p
            | Shift -> c.shift p
        | None -> c

    let getVolCurve ins o = 
        let c = getVols ins
        match o with 
        | Some (m,p) -> 
            match m with
            | Flatten -> c.flatten p
            | Shift -> c.shift p
        | None -> c

    let getCurveUnit (PriceCurve p) = p |> Map.toSeq |> Seq.head |> snd |> getCaseDecimal |> fst            

    let getTTM (pd:DateTime) (d:DateTime)= max ((d - pd ).TotalDays / 365.) 0.
    //let getTTM expDate (pricingDate:DateTime) d getTTMM' pricingDate ( min d expDate ) 
