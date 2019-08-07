namespace Commod
///Define swap and futures data type and pricing
module Swaps = 
    open System
    open Utils
    open Markets
    open Deedle
    open Calendars

    let brtFutures = getCommod 1.<USD/bbl> 1000.<bbl/lot> BRT
    let jccIndex = getCommod 1.<USD/bbl> 1000.<bbl/lot> JCC
    let jkmIndex = getCommod 1.<USD/mmbtu> 10000.<mmbtu/lot> JKM
    let ttfIndex = getCommod 1.<USD/mmbtu> 10000.<mmbtu/lot> TTF

    type FutureContract<[<Measure>] 'c, [<Measure>] 'u> = 
        { 
            fut:Commod<'c,'u>
            ContractMonth: string
            quantity: float<lot>
            fixedPrice: float<'c/'u>
        }
    type PriceCurve<[<Measure>]'u> = PriceCurve of Series<string, float<'u>> //prices with quotation

    type FutureContractPricer<[<Measure>]'c,[<Measure>]'u> = FutureContract<'c,'u> -> PriceCurve<'c/'u> -> float<'c> //function types

    let genericFuturePricer (f:FutureContract<_,_>) p =
        let p0 = p |> Series.get f.ContractMonth
        ( p0  - f.fixedPrice) * (f.fut.LotSize * f.quantity)

    type AverageFrequency = BusinessDays
    //type PeriodFrequency = |CalMonth  //allow broken period both ends
    type AverageSpecs<[<Measure>]'c,[<Measure>]'u>=
        {
           Commod: Commod<'c,'u>
           Frequency: AverageFrequency       
           RollAdj : int
           Nrby: int
        }

    type PeriodSpecs<[<Measure>]'c,[<Measure>]'u> = 
        { 
             startDate:DateTime
             endDate: DateTime
             deliveryDate: DateTime    
             nominal: float<'u>
             strike: float<'c/'u>
        }

    type AverageSwap<[<Measure>]'c,[<Measure>]'u>=
        {
           AverageSpecs: AverageSpecs<'c,'u>
           PeriodSpecs: seq<PeriodSpecs<'c,'u>>
        }

    let getFixingDates freq hols d1 d2 = 
        match freq with
        | BusinessDays -> bdRange hols d1 d2

    ///roll and nrby adjust the contracts
    let getNrbyContracts (s:AverageSpecs<_,_>) = 
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
            Commod = brtFutures
            Frequency = BusinessDays
            RollAdj = 1
            Nrby = 0
        }

    let jkmAvgFwd = 
        {
            Commod = jkmIndex
            Frequency = BusinessDays
            RollAdj = 0
            Nrby = 0
        }

    let ttfAvgFwd = 
        {
            Commod = ttfIndex
            Frequency = BusinessDays
            RollAdj = 0
            Nrby = 0
        }

    let brtAvgFwd0 = {brtAvgFwd with RollAdj = 0 }

    let getbrtswap d1 d2 nominal strike  = //generate standard swap
        let dates = generateCalMonthSchedule d1 d2 |> Seq.map( fun (d1,d2) -> (d1, d2, (dateAdjust calendars.[USD] "5b" d2)))
        {
            AverageSpecs = brtAvgFwd
            PeriodSpecs = 
                dates 
                |> Seq.map( fun (d1,d2,d3) -> { startDate = d1; endDate = d2; deliveryDate = d3; nominal = nominal; strike = strike } )
        }

    let getbrtswapbyPeriod period nominal strike =
        let (d1, d2 ) = getPeriod period
        getbrtswap d1 d2 nominal strike

    let getFixingDatesFromAvg (s:AverageSpecs<_,_>) d1 d2= 
        getFixingDates s.Frequency s.Commod.Calendar d1 d2

    ///avg swap's pillar dependencies
    let depPillar (s:AverageSpecs<_,_>) d1 d2 =         
        let fixingDates = getFixingDatesFromAvg s d1 d2
        let contractDates = getNrbyContracts s
        getFixingContracts contractDates fixingDates 
        |> set

    //used to filter out unnecessary pillars before pricing and risk computation
    let depCurv pillars (PriceCurve p) = 
        p |> Series.filter( fun k _ -> Set.contains k pillars) |> PriceCurve


    let priceSwap (s:AverageSwap<_,_>) p = 
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
            let avg = getFixingPrices contractDates fixingDates p |> Seq.average 
            (avg - period.strike) * period.nominal
            )

