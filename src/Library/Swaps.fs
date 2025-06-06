namespace Commod

///Define swap and futures data type and pricing
[<AutoOpen>]
module Swaps =
    open System

    //TODO: fix general case
    /// Prices a generic future contract.
    /// f: The future contract to price.
    /// PriceCurve p: The price curve.
    /// Returns the price of the future contract.
    let genericFuturePricer (f: FutureContract) (PriceCurve p) =
        let qty = f.Fut.LotSize.Case
        let q = f.Fut.LotSize * (f.Quantity / 1M<lot>)
        //get price
        let p0 = p.Item f.ContractMonth |> convertUnitPrice qty f.Fut
        let k0 = f.FixedPrice |> convertUnitPrice qty f.Fut
        //convert to common units using futures contract members
        let diff: UnitPrice = p0 - k0
        diff * q

    type AverageFrequency =
        | BusinessDays
        | LastBD

    //type PeriodFrequency = |CalMonth  //allow broken period both ends
    /// Defines the specifications for an average price swap.
    type AverageSpecs =
        { /// The underlying commodity.
          Commod: Commod
          /// The frequency of averaging.
          Frequency: AverageFrequency
          /// The roll adjustment.
          RollAdj: int
          /// The nearby contract number.
          Nrby: int }

    /// Defines the specifications for a period within an average price swap.
    type PeriodSpecs =
        { /// The start date of the period.
          startDate: DateTime
          /// The end date of the period.
          endDate: DateTime
          /// The delivery date of the period.
          deliveryDate: DateTime
          /// The nominal quantity for the period.
          nominal: QuantityAmount
          /// The strike price for the period.
          strike: UnitPrice }

    /// Defines an average price swap.
    type AverageSwap =
        { /// The specifications for the average price swap.
          AverageSpecs: AverageSpecs
          /// The array of period specifications.
          PeriodSpecs: PeriodSpecs[] }

        member this.Quantity =
            this.PeriodSpecs |> Array.map (fun s -> s.nominal) |> Array.reduce (+)

    /// Gets the fixing dates for a given frequency, holidays, start date, and end date.
    /// freq: The averaging frequency.
    /// hols: The holiday calendar.
    /// d1: The start date.
    /// d2: The end date.
    /// Returns an array of fixing dates.
    let getFixingDates freq hols d1 d2 =
        match freq with
        | BusinessDays -> bdRange hols d1 d2
        | LastBD -> [| dateAdjust hols "p" d2 |]

    /// Gets the nearby contracts based on the average specifications.
    /// s: The average specifications.
    /// Returns the adjusted contract dates.
    let getNrbyContracts (s: AverageSpecs) =
        let hols = s.Commod.Calendar
        let rolladj = s.RollAdj
        let nrby = s.Nrby
        let (ContractDates cnts) = s.Commod.Contracts

        if rolladj < 0 then
            (invalidArg "rolladj" "invalid rolladj or nrby number, expect positive int")

        if nrby < 0 then
            (invalidArg "nrby" "invalid nrby number, expect positive int")

        cnts
        |> Map.toArray
        |> Array.map (fun (k, (v, o)) ->
            let k' = (pillarToDate k).AddMonths nrby |> formatPillar
            let v' = addBusinessDay -rolladj hols (dateAdjust hols "p" v)
            k', (v', o))
        |> Map.ofArray
        |> ContractDates

    /// Gets the contract month for a given set of contract dates and a contract identifier.
    /// c: The contract dates.
    /// cnt: The contract identifier (pillar).
    /// Returns a tuple of the start and end dates of the contract month.
    let getContractMonth (c: ContractDates) cnt =
        let prior =
            match cnt with
            | MMMYY d -> d.AddMonths(-1) |> formatPillar
            | _ -> failwith "expect MMMyy format pillar only"

        c.[prior].AddDays(1.), c.[cnt]

    /// Gets the fixing contracts for a given set of contract dates and an array of dates.
    /// ContractDates c: The contract dates.
    /// dates: An array of dates.
    /// Returns an array of pillar strings representing the fixing contracts.
    let getFixingContracts (ContractDates c) dates =
        //cnts could be after roll/nrby adj, return the pillar used to lookup price, so we know the exact dependencies and also enable diffsharp can work
        let s =
            c |> Map.toArray |> Array.map (fun (k, (v, _)) -> (v, k)) |> Array.sortBy fst

        dates |> Array.map (fun d -> s |> Array.find (fun x -> fst x >= d) |> snd)

    /// Gets the fixing prices for a given set of contract dates, an array of dates, a price curve, an instrument, and a pricing date.
    /// c: The contract dates.
    /// dates: An array of dates.
    /// PriceCurve p: The price curve.
    /// ins: The instrument.
    /// pd: The pricing date.
    /// Returns an array of fixing prices.
    let getFixingPrices c dates (PriceCurve p) (ins: Instrument) pd = //cnts should be after roll/nrby adj
        let unit, _ = getCaseDecimal (p.Values |> Seq.head)

        let past, future =
            match getfixing ins pd with
            | Some _ -> (dates |> Array.filter (fun x -> x <= pd)), (dates |> Array.filter (fun x -> x > pd))
            | None -> (dates |> Array.filter (fun x -> x < pd)), (dates |> Array.filter (fun x -> x >= pd))

        let p1 = 
            match past with 
            | [||] -> [||]
            | x -> x |> getfixings ins |> Array.map (applyCaseDecimal unit)

        let p2 =
            getFixingContracts c future
            |> Array.map (fun k ->
                let v = Map.tryFind k p

                match v with
                | Some x -> x
                | _ ->
                    (sprintf "Please check market data input, missing pillar %s from curve %A" k p)
                    |> invalidOp)

        Array.append p1 p2

    /// Gets the average forward specifications for a given instrument.
    /// ins: The instrument.
    /// Returns the average forward specifications.
    let getAvgFwd ins =
        let avg =
            { Commod = getCommod ins
              Frequency = BusinessDays
              RollAdj = 0
              Nrby = 0 }

        match ins with
        | BRT -> { avg with RollAdj = 1 }
        | _ -> avg

    //let brtAvgFwd = getAvgFwd BRT
    //let dbrtAvgFwd = getAvgFwd DBRT
    //let jkmAvgFwd = getAvgFwd JKM
    //let ttfAvgFwd = getAvgFwd TTF
    //let brtAvgFwd0 = {brtAvgFwd with RollAdj = 0 }

    /// Generates a standard swap for a given instrument, start date, end date, nominal, and strike.
    /// ins: The instrument.
    /// d1: The start date.
    /// d2: The end date.
    /// nominal: The nominal quantity.
    /// strike: The strike price.
    /// Returns the generated average swap.
    let getSwap ins d1 d2 nominal strike = //generate standard swap
        let avg = getAvgFwd ins
        let cnts = avg.Commod.Contracts

        let getPeridRange (d1, d2) =
            match ins with
            | TTF -> //bullet
                let d = cnts.[(formatPillar d2)]
                d, d
            | JKM -> getContractMonth cnts (formatPillar d2)
            | _ -> d1, d2 //default calmonth

        let dates =
            generateCalMonthSchedule d1 d2
            |> Array.map getPeridRange
            |> Array.map (fun (d1, d2) -> (d1, d2, (dateAdjust avg.Commod.Calendar "5b" d2)))

        { AverageSpecs = avg
          PeriodSpecs =
            dates
            |> Array.map (fun (d1, d2, d3) ->
                { startDate = d1
                  endDate = d2
                  deliveryDate = d3
                  nominal = nominal
                  strike = strike }) }

    /// Generates a standard Brent swap.
    /// d1: The start date.
    /// d2: The end date.
    /// nominal: The nominal quantity.
    /// strike: The strike price.
    /// Returns the generated Brent average swap.
    let getbrtswap d1 d2 nominal strike = //generate standard swap
        getSwap BRT d1 d2 nominal strike

    /// Generates a standard Brent swap for a given period, nominal, and strike.
    /// period: The period string.
    /// nominal: The nominal quantity.
    /// strike: The strike price.
    /// Returns the generated Brent average swap.
    let getbrtswapbyPeriod period nominal strike =
        let (d1, d2) = getPeriod period
        getbrtswap d1 d2 nominal strike

    /// Gets the fixing dates from average specifications, start date, and end date.
    /// s: The average specifications.
    /// d1: The start date.
    /// d2: The end date.
    /// Returns an array of fixing dates.
    let getFixingDatesFromAvg (s: AverageSpecs) d1 d2 =
        getFixingDates s.Frequency s.Commod.Calendar d1 d2

    /////avg swap's pillar dependencies
    //let depPillar (s:AverageSpecs) d1 d2 =
    //    let fixingDates = getFixingDatesFromAvg s d1 d2
    //    let contractDates = getNrbyContracts s
    //    getFixingContracts contractDates fixingDates
    //    |> set

    ////used to filter out unnecessary pillars before pricing and risk computation
    //let depCurv pillars (PriceCurve p) =
    //    p |> Map.filter( fun k _ -> Set.contains k pillars) |> PriceCurve

    /// Prices an average swap.
    /// s: The average swap to price.
    /// p: The price curve.
    /// pd: The pricing date.
    /// Returns the price of the swap.
    let priceSwap (s: AverageSwap) p pd =
        let contractDates = getNrbyContracts s.AverageSpecs
        let ins = s.AverageSpecs.Commod.Instrument

        s.PeriodSpecs
        |> Array.map (fun period ->
            //let activePillars = depPillar s.AverageSpecs period.startDate period.endDate
            //let p' = depCurv activePillars p
            let fixingDates =
                getFixingDatesFromAvg s.AverageSpecs period.startDate period.endDate

            let avg = getFixingPrices contractDates fixingDates p ins pd |> avgPrice
            let v = (avg - period.strike)
            v * period.nominal)
        |> Array.reduce (+)
