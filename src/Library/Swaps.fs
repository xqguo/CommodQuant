namespace Commod
///Define swap and futures data type and pricing
module Swaps = 
    open System
    open Utils
    open Markets
    open Deedle
    open Calendars

    let diffUnitPrice (p1:UnitPrice) (p2:UnitPrice) = 
        match (p1, p2) with
        | USDBBL p1 , USDBBL p2 -> p1 - p2 |> USDBBL
        | USDMT p1, USDMT p2 -> p1 - p2 |> USDMT
        | _ -> invalidOp "Inconsistent unit"
    
    let diffAmount p1 p2 = 
        match (p1, p2) with
        | USD p1, USD p2 -> p1 - p2 |> USD
        | EUR p1, EUR p2 -> p1 - p2 |> EUR
        | _ -> invalidOp "Inconsistent unit"

    ///get ccy amount with baked unit conversion
    let rec getPrice (p:UnitPrice) (q:QuantityAmount) (c:Commod)= 
        match (p, q) with
        | USDBBL p , BBL q -> p * q |> CurrencyAmount.USD
        | USDMT p, MT q -> p * q |> CurrencyAmount.USD
        | _ -> 
            match c.Instrument with 
            | FO180 ->                 
                match (p, q) with
                | USDBBL _ , MT _ -> 
                    let q1 = applyMT 1M
                    let q2 = applyBBL 1M
                    let q = unitConversion q1 q2 c
                    getPrice p q c
                |_ -> invalidOp "not implemented"
        
    let genericFuturePricer (f:FutureContract) (PriceCurve p) =
        //get price
        let p0 = p |> Series.get f.ContractMonth
        let q = 
            match f.fut.LotSize with
            | BBL x -> f.quantity / 1M<lot> * x |> BBL
            | _ -> invalidOp "futures quantity should be in lots"
        //convert to common units using futures contract members
        let leg1 = getPrice p0 q f.fut
        let leg2 = getPrice f.fixedPrice q f.fut
        diffAmount leg1 leg2 

    type AverageFrequency = BusinessDays
    //type PeriodFrequency = |CalMonth  //allow broken period both ends
    type AverageSpecs=
        {
           Commod: Commod
           Frequency: AverageFrequency       
           RollAdj : int
           Nrby: int
        }

    type PeriodSpecs = 
        { 
             startDate:DateTime
             endDate: DateTime
             deliveryDate: DateTime    
             nominal: QuantityAmount
             strike: UnitPrice
        }

    type AverageSwap=
        {
           AverageSpecs: AverageSpecs
           PeriodSpecs: seq<PeriodSpecs>
        }

    let getFixingDates freq hols d1 d2 = 
        match freq with
        | BusinessDays -> bdRange hols d1 d2

    ///roll and nrby adjust the contracts
    let getNrbyContracts (s:AverageSpecs) = 
        let hols =  s.Commod.Calendar
        let rolladj = s.RollAdj
        let nrby = s.Nrby
        let (ContractDates cnts) = s.Commod.Contracts
        if rolladj < 0 then (invalidArg "rolladj" "invalid rolladj or nrby number, expect positive int")
        if nrby < 0 then (invalidArg "nrby" "invalid nrby number, expect positive int")
        cnts |> Series.shift nrby |> Series.mapValues (addBusinessDay -rolladj hols ) |> ContractDates

    let getFixingContracts (ContractDates c) dates =  
        //cnts could be after roll/nrby adj, return the pillar used to lookup price, so we know the exact dependencies and also enable diffsharp can work
        let s = c |> Series.observations |> Seq.map ( fun (k,v) -> (v, k))|> series
        dates |> Seq.map( fun d -> s |> Series.lookup d Lookup.ExactOrGreater )

    let getFixingPrices c dates p =  //cnts should be after roll/nrby adj
        getFixingContracts c dates
        |> Seq.map( fun k -> 
            let v = Series.tryGet k p
            match v with
            | Some x -> x
            | _ -> (sprintf "Please check market data input, missing pillar %s from curve %A" k p) |> invalidOp
            )
    let brtAvgFwd = 
        {
            Commod = getCommod BRT
            Frequency = BusinessDays
            RollAdj = 1
            Nrby = 0
        }

    let jkmAvgFwd = 
        {
            Commod = getCommod JKM
            Frequency = BusinessDays
            RollAdj = 0
            Nrby = 0
        }

    let ttfAvgFwd = 
        {
            Commod = getCommod TTF
            Frequency = BusinessDays
            RollAdj = 0
            Nrby = 0
        }

    let brtAvgFwd0 = {brtAvgFwd with RollAdj = 0 }

    let getbrtswap d1 d2 nominal strike  = //generate standard swap
        let dates = generateCalMonthSchedule d1 d2 |> Seq.map( fun (d1,d2) -> (d1, d2, (dateAdjust calendars.[CME] "5b" d2)))
        {
            AverageSpecs = brtAvgFwd
            PeriodSpecs = 
                dates 
                |> Seq.map( fun (d1,d2,d3) -> { startDate = d1; endDate = d2; deliveryDate = d3; nominal = nominal; strike = strike } )
        }

    let getbrtswapbyPeriod period nominal strike =
        let (d1, d2 ) = getPeriod period
        getbrtswap d1 d2 nominal strike

    let getFixingDatesFromAvg (s:AverageSpecs) d1 d2= 
        getFixingDates s.Frequency s.Commod.Calendar d1 d2

    ///avg swap's pillar dependencies
    let depPillar (s:AverageSpecs) d1 d2 =         
        let fixingDates = getFixingDatesFromAvg s d1 d2
        let contractDates = getNrbyContracts s
        getFixingContracts contractDates fixingDates 
        |> set

    //used to filter out unnecessary pillars before pricing and risk computation
    let depCurv pillars (PriceCurve p) = 
        p |> Series.filter( fun k _ -> Set.contains k pillars) |> PriceCurve

    ///TODO: fix pricing.
    let priceSwap (s:AverageSwap) p = 
        // let activePillars = 
        //     s.PeriodSpecs 
        //     |> Seq.map( fun period -> depPillar s.AverageSpecs period.startDate period.endDate )
        //     |> Set.unionMany

        // let p' = depCurv activePillars p

        s.PeriodSpecs 
        |> Seq.sumBy( fun period -> 
            let activePillars = depPillar s.AverageSpecs period.startDate period.endDate        
            let fixingDates = getFixingDatesFromAvg s.AverageSpecs period.startDate period.endDate 
    //        printfn "%A" (fixingDates |> List.ofSeq)
            let contractDates = getNrbyContracts s.AverageSpecs
            //printfn "%A" contractDates
            //let avg = getFixingPrices contractDates fixingDates p |> Seq.average 
            //(avg - period.strike) * period.nominal
            0.
            )

