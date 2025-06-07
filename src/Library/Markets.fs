namespace Commod

[<AutoOpen>]
module Markets =
    open IOcsv
    open System
    open Deedle
    open FSharp.Data
    //cache the commods as it involves disk IO
    let commoddict =
        new System.Collections.Concurrent.ConcurrentDictionary<Instrument, Commod>()

    /// <summary>
    /// A dictionary of conversion factors between commodity units.
    /// </summary>
    /// <remarks>
    /// The key is a tuple of (Instrument, from unit, to unit), and the value is the conversion factor as a decimal.
    /// </remarks>
    let conversionFactors =
        [
          //ins, from, to
          (FO180, "MT", "BBL"), 6.35M
          (FO380, "MT", "BBL"), 6.35M ]
        |> dict

    /// <summary>
    /// Gets the conversion factor between two units for a given instrument.
    /// </summary>
    /// <param name="i">The instrument.</param>
    /// <param name="qty">The source unit (e.g., "MT").</param>
    /// <param name="qty2">The target unit (e.g., "BBL").</param>
    /// <returns>The conversion factor as a decimal.</returns>
    /// <exception cref="InvalidOperationException">Thrown if no conversion factor is found.</exception>
    let getConversion i qty qty2 =
        let key = (i, qty, qty2)

        if conversionFactors.ContainsKey key then
            conversionFactors.[key]
        else
            let key' = (i, qty2, qty)

            if conversionFactors.ContainsKey key' then
                1M / conversionFactors.[key']
            else
                invalidOp
                <| sprintf "Conversion factor to %s from %s not found for %A" qty2 qty i

    /// <summary>
    /// Converts a quantity amount to a different unit for a given commodity.
    /// </summary>
    /// <param name="f">The original quantity amount.</param>
    /// <param name="qty2">The target unit.</param>
    /// <param name="i">The commodity.</param>
    /// <returns>The converted quantity amount.</returns>
    let qtyConversion (f: QuantityAmount) (qty2: string) (i: Commod) =
        let qty, x = getCaseDecimal f

        if qty = qty2 then
            f
        else
            let y = getConversion i.Instrument qty qty2
            x * y |> QuantityAmount.applyCase qty2

    /// <summary>
    /// Converts a unit price to be consistent with a target quantity unit based on commodity conversion factors.
    /// </summary>
    /// <param name="qty2">The target quantity unit (e.g., "BBL").</param>
    /// <param name="c">The commodity.</param>
    /// <param name="p">The original unit price.</param>
    /// <returns>The converted unit price.</returns>
    let convertUnitPrice (qty2: string) (c: Commod) (p: UnitPrice) =
        let quote, x = getCaseDecimal p
        let ccy1 = quote.Substring(0, 3)
        let qty1 = quote.Substring 3

        if qty1 = qty2 then
            p //no need to convert
        else
            let q1 = QuantityAmount.applyCase qty1 1M
            let r = qtyConversion q1 qty2 c |> getCaseDecimal |> snd
            x * r |> UnitPrice.applyCase (ccy1 + qty2)

    /// <summary>
    /// Applies a function to a quantity amount, converting to a target unit for a given commodity first.
    /// </summary>
    /// <param name="f">The function to apply to the decimal value.</param>
    /// <param name="q1">The original quantity amount.</param>
    /// <param name="q2">The target quantity amount (for unit reference).</param>
    /// <param name="c">The commodity.</param>
    /// <returns>The result as a quantity amount in the target unit.</returns>
    let mapQuantity f (q1: QuantityAmount) (q2: QuantityAmount) (c: Commod) =
        let qty2 = q2 |> getCaseDecimal |> fst
        let x = qtyConversion q1 qty2 c |> getCaseDecimal |> snd
        f x |> QuantityAmount.applyCase qty2

    /// <summary>
    /// Converts a quantity amount to lots for a given commodity.
    /// </summary>
    /// <param name="q">The original quantity amount.</param>
    /// <param name="i">The commodity.</param>
    /// <returns>The number of lots as a quantity amount.</returns>
    let lotsConversion (q: QuantityAmount) (i: Commod) =
        let getLots x = x / i.Lot
        mapQuantity getLots q i.LotSize i

    /// <summary>
    /// Applies a unit case to all values in a map of unit prices.
    /// </summary>
    /// <param name="case">The unit case to apply (e.g., "USD/BBL").</param>
    /// <param name="s">The map of unit prices.</param>
    /// <typeparam name="'a">The type of the map keys.</typeparam>
    /// <returns>A map with the unit case applied to all values.</returns>
    let inline applyMapUnit case s =
        s |> Map.map (fun k v -> UnitPrice.applyCase case v)

    /// <summary>
    /// Creates a commodity object from an instrument.
    /// </summary>
    /// <returns>A new commodity object for the given instrument.</returns>
    let createCommod =
        let getCommod' ins =
            let (q, s) =
                match ins with
                | BRT
                | DBRT
                | JCC
                | DUB
                | SJET
                | SGO -> USDBBL 1M<USD / bbl>, BBL 1000M<bbl>
                | FO180
                | FO380
                | MFO
                | FO35
                | GO -> USDMT 1M<USD / mt>, MT 1000M<mt>
                | NG
                | JKM
                | NBP
                | TTF
                | SPP
                | _ -> USDMMBTU 1M<USD / mmbtu>, MMBTU 10000M<mmbtu>

            let cals = getCalendar ins
            let contracts = getContracts ins

            { Instrument = ins
              Calendar = cals
              Contracts = contracts
              Quotation = q
              LotSize = s }

        fun ins -> getCommod' ins

    /// <summary>
    /// get a commod object from an instrument, cached for performance
    /// </summary>
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
            |> Seq.map (fun (p, x) ->
                let pillar =
                    match p with
                    | "TODAY" -> "TODAY"
                    | s when s.StartsWith "BOM" -> "BOM"
                    | x -> pillarToDate x |> formatPillar

                pillar, decimal x)
            |> Seq.filter (fun (p, _) -> c.ContainsKey p || p = "TODAY" || p = "BOM")
            |> Map.ofSeq
            |> applyMapUnit i.Quotation.Case
            |> PriceCurve
        | None -> invalidOp <| sprintf "Cannot load prices for %A" ins

    /// <summary>
    /// create a vol curve from a sequence of pillar, vol pairs
    /// vol is in percent, e.g. 20.1 for 20.1%
    /// </summary>
    let pctvolcurve (data: seq<string * float>) =
        data
        |> Seq.map (fun (p, v) ->
            let pillar =
                match p with
                | "TODAY" -> "TODAY"
                | s when s.StartsWith "BOM" -> "BOM"
                | x -> pillarToDate x |> formatPillar

            pillar, (PercentVol(decimal v)))
        |> Map.ofSeq
        |> VolCurve

    /// <summary>
    /// create a vol curve from a sequence of pillar, vol pairs
    /// vol is in absolute value
    /// </summary>
    let absvolcurve (data: seq<string * float>) =
        data
        |> Seq.map (fun (p, v) ->
            let pillar =
                match p with
                | "TODAY" -> "TODAY"
                | s when s.StartsWith "BOM" -> "BOM"
                | x -> pillarToDate x |> formatPillar

            pillar, (AbsoluteVol(decimal v)))
        |> Map.ofSeq
        |> VolCurve
    // these depends on the data format
    // commod curve pillars are either MMM-yy or TODAY or BOM, all in upper case.
    // vols data in market quote ( percent ), e.g. 20.1
    /// <summary>
    /// get a vol curve for an instrument
    /// </summary>
    let getVols ins =
        let i = getCommod ins
        let (ContractDates c) = i.Contracts
        let f = tryVolsFile ins

        match f with
        | Some v ->
            //let data = PriceCsv.AsyncLoad v |> Async.RunSynchronously
            //data.Rows
            getPrice v
            |> Seq.filter (fun (p, _) -> c.ContainsKey p || p = "TODAY" || p = "BOM")
            |> pctvolcurve
        | None -> invalidOp <| sprintf "Cannot load prices for %A" ins

    // commod curve pillars are in MMM-yy
    // vols data in market quote ( percent ), e.g. 20.1
    /// <summary>
    /// get a smile from a csv file
    /// csv format is: pillar, delta1, delta2, ...
    /// each row is a pillar with vols for different deltas
    /// </summary>
    let getSmileFromCSV v =
        let getnum x =
            x |> Array.skip 1 |> Array.map (fun s -> (Double.Parse s) / 100.)

        let data = CsvFile.AsyncLoad v |> Async.RunSynchronously

        let deltas =
            match data.Headers with
            | Some h -> h |> getnum
            | None -> failwith "wrong smile file format"

        let (pillars, vols) =
            data.Rows
            |> Array.ofSeq
            |> Array.map (fun r ->
                let p = r.Columns |> Array.head
                let v = r.Columns |> getnum
                p, v)
            |> Array.unzip

        let fp = pillars |> Array.map (pillarToDate >> formatPillar)
        new VolDeltaSmile(fp, deltas, vols)

    /// <summary>
    /// get a smile for an instrument
    /// </summary>
    let getSmile ins =
        let f = trySmileFile ins

        match f with
        | Some v -> getSmileFromCSV v
        | None -> invalidOp <| sprintf "Cannot load smile for %A" ins

    type CurveOveride =
        | Flatten
        | Shift

    /// <summary>
    /// get a price curve for an instrument
    /// </summary>
    /// <param name="ins">instrument</param>
    /// <param name="o">optional curve overide, flatten or shift by a value</param>
    let getPriceCurve ins o =
        let c = getPrices ins

        match o with
        | Some(m, p) ->
            match m with
            | Flatten -> c.flatten p
            | Shift -> c.shift p
        | None -> c

    /// <summary>
    /// Gets a volatility curve for an instrument, with optional curve override (flatten or shift).
    /// </summary>
    /// <param name="ins">The instrument for which to get the volatility curve.</param>
    /// <param name="o">Optional curve override: flatten or shift by a value.</param>
    /// <returns>A volatility curve, possibly modified by the override.</returns>
    let getVolCurve ins o =
        let c = getVols ins
        match o with
        | Some(m, p) ->
            match m with
            | Flatten -> c.flatten p
            | Shift -> c.shift p
        | None -> c

    /// <summary>
    /// Gets the unit of a price curve, e.g. USD/BBL.
    /// </summary>
    /// <param name="p">A price curve object containing price data.</param>
    /// <returns>The unit string, such as "USD/BBL".</returns>
    let getCurveUnit (PriceCurve p) =
        p |> Map.toSeq |> Seq.head |> snd |> getCaseDecimal |> fst

    /// <summary>
    /// Gets the time to maturity in years between two dates.
    /// </summary>
    /// <param name="pd">The pricing date.</param>
    /// <param name="d">The maturity date.</param>
    /// <returns>Time to maturity in years (non-negative).</returns>
    let getTTM (pd: DateTime) (d: DateTime) = max ((d - pd).TotalDays / 365.) 0.

    /// <summary>
    /// Reloads market data sources and clears cached data.
    /// </summary>
    /// <remarks>
    /// This function resets the USD OIS source, reloads fixings, and clears commodity and holiday caches.
    /// </remarks>
    let reload () =
        USDOISSOURCE <- (ROOT +/ "csv" +/ "USD OIS_Rate.csv")
        fixings <-
            tryFile (ROOT +/ "csv" +/ "fixings.csv")
            |> Option.map (fun f -> Frame.ReadCsv(f) |> Frame.indexRowsDate "Date")
        commoddict.Clear()
        hols.Clear()
