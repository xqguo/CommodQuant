namespace Commod

///Define swap and futures data type and pricing
[<AutoOpen>]
module Swaps =
    open System

    //TODO: fix general case
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
    type AverageSpecs =
        { Commod: Commod
          Frequency: AverageFrequency
          RollAdj: int
          Nrby: int }

    type PeriodSpecs =
        { startDate: DateTime
          endDate: DateTime
          deliveryDate: DateTime
          nominal: QuantityAmount
          strike: UnitPrice }

    type AverageSwap =
        { AverageSpecs: AverageSpecs
          PeriodSpecs: PeriodSpecs[] }

        member this.Quantity =
            this.PeriodSpecs |> Array.map (fun s -> s.nominal) |> Array.reduce (+)

    let getFixingDates freq hols d1 d2 =
        match freq with
        | BusinessDays -> bdRange hols d1 d2
        | LastBD -> [| dateAdjust hols "p" d2 |]

    ///roll and nrby adjust the contracts
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

    let getContractMonth (c: ContractDates) cnt =
        let prior =
            match cnt with
            | MMMYY d -> d.AddMonths(-1) |> formatPillar
            | _ -> failwith "expect MMMyy format pillar only"

        c.[prior].AddDays(1.), c.[cnt]

    let getFixingContracts (ContractDates c) dates =
        //cnts could be after roll/nrby adj, return the pillar used to lookup price, so we know the exact dependencies and also enable diffsharp can work
        let s =
            c |> Map.toArray |> Array.map (fun (k, (v, _)) -> (v, k)) |> Array.sortBy fst

        dates |> Array.map (fun d -> s |> Array.find (fun x -> fst x >= d) |> snd)

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

    let getbrtswap d1 d2 nominal strike = //generate standard swap
        getSwap BRT d1 d2 nominal strike

    let getbrtswapbyPeriod period nominal strike =
        let (d1, d2) = getPeriod period
        getbrtswap d1 d2 nominal strike

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
