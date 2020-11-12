namespace Commod
[<AutoOpen>]
module Pricer =
    open System
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Statistics

    //let SwapPricer inst start end =
    //    ()

    let SpreadOptionPricer inst1 start1 end1 inst2 start2 end2 slope freight callput expDate  
        refMonth (pricingDate:DateTime)
        rho pricecurve1 volcurve1 pricecurve2 volcurve2 price1 vol1 price2 vol2 =
        let getTTM expDate d = max ((( min d expDate ) - pricingDate ).TotalDays / 365.) 0.

        // get equal weights based on the number of fixings
        let getEqualWeights x =
            let n = List.length x
            let w = 1.0 / float n 
            List.replicate n w 

        let lags1 = [start1 .. end1]
        let lags2 = [start2 .. end2]
        let inline toVector s = s |> Seq.map float |> List.ofSeq |> vector 

        /// split fixings into future and past using pricingDate
        let splitDetails dates weights contracts = 
            let details = List.zip3 dates weights contracts
            let n = dates |> List.tryFindIndex( fun x -> x > pricingDate ) 
            match n with
            | Some 0 -> (List.empty, details)
            | Some i -> List.splitAt i details
            | None -> (details, List.empty)

        ///take refmonth and return a tuple of 3 lists: 
        ///fixingdate, contract, weight, s
        ///lope is applied here into the weights.
        let getFixings refMonth (com:Commod) lags slope =     
            let refDate = refMonth |> pillarToDate 
            lags 
            |> List.map( fun i -> 
                let refMonth = refDate.AddMonths i 
                let contract = refMonth |> formatPillar
                //get reference contract, swap for oil, bullet for gas
                let avgfwd = getAvgFwd com.Instrument
                let d1,d2 = 
                    match com.Instrument with 
                    | BRT 
                    | DBRT -> 
                        refMonth, dateAdjust' "e" refMonth
                    | _ -> //for gas, use the 1d bullet on expiration date
                        let (ContractDates cnts) = com.Contracts
                        let d = cnts.[contract]
                        d,d

                let dates = getFixingDates avgfwd.Frequency com.Calendar d1 d2 |> Seq.toList
                //let contracts = List.replicate dates.Length contract
                let contracts = getFixingContracts (getNrbyContracts avgfwd) dates |> Seq.toList
                let weights = (getEqualWeights dates) |> List.map( fun x -> x /(float lags.Length) * (float slope))
                dates, contracts, weights
                )
            |> List.reduce( fun ( d1,c1,w1) (d2,c2,w2) -> 
                (List.append d1 d2), 
                (List.append c1 c2),
                (List.append w1 w2))

        ///generate inputs for option pricing 
        /// inputs are
        /// futureDetails is a list of tuple of fixingdate, weight, ContractPillar 
        /// getPriceFunc take contractPillar and return price 
        /// getVolFunc take contractPillar and return vol
        /// return tuple of 4 vectors:
        /// forwards, weights, time to maturity, vols
        let getFutureInputs futureDetails' getPriceFunc getVolFunc (expDate:DateTime) = 
            //consolidate future details to group weightes for same fixing dates and same contracts
            let futureDetails= 
                futureDetails'
                |> List.map( fun ( d, w, c ) -> (getTTM expDate d ), w , c ) 
                |> List.groupBy(fun (x,_,z) -> x,z) |> List.map( fun ((k1,k2),v) -> k1,(v |> List.sumBy( fun (_,x,_)->x)),k2)

            let fw1 = 
                if List.isEmpty futureDetails then 
                    Vector<float>.Build.Dense(1) + 0.00001
                else 
                    futureDetails |> List.map( fun ( _, w, _ ) -> w) |> vector
            let t1 = 
                if futureDetails.IsEmpty then 
                    Vector<float>.Build.Dense(1) + 0.00001
                else 
                    futureDetails |> List.map( fun ( d, _, _ ) -> d ) |> vector

            let contracts = futureDetails |> List.unzip3 |> fun ( _, _, c ) -> c 
            let f1 = 
                if futureDetails.IsEmpty then 
                    Vector<float>.Build.Dense(1) + 0.00001
                else 
                    contracts |> List.map getPriceFunc  |> toVector
            let v1 = 
                if futureDetails.IsEmpty then 
                    Vector<float>.Build.Dense(1) + 0.00001
                else 
                    contracts |> List.map getVolFunc |> toVector
            (f1, fw1, t1, v1)

        ///generate empty past quickly for use when the deal has not past dependency
        let emptyPastInputs = 
            let p = Vector<float>.Build.Dense(1)
            ( p , p )

        ///generate past fixings required for asian option
        let getPastInputs pastDetails getFixingFunc = 
                if List.isEmpty pastDetails then 
                    emptyPastInputs
                else 
                    let w = pastDetails |> List.unzip3 |> fun ( _, w, _ ) -> w |> vector
                    let p = pastDetails |> List.map(fun ( d, _, c ) -> getFixingFunc d c ) |> toVector
                    ( w, p )            

        //for the final portfolio we need just functions that take price curve and return price. 

        let getInputs expDate refMonth lags inst slope (pricecurve:PriceCurve) (volcurve:VolCurve) priceOverride volOverride = 
            let com = getCommod inst
            let getPrices1 c = 
               match priceOverride with
               | Some v -> v
               | None ->
                    if pricecurve.Pillars.Contains c then
                        (pricecurve.Item c).Value
                    else
                        failwithf "try to getPrice:%s from %A" c pricecurve
            let getVol c = 
               match volOverride with
               | Some v -> v
               | None -> 
                    if volcurve.Pillars.Contains c then
                        volcurve.Item c
                    else
                        failwithf "try to get vol:%s from %A" c volcurve

            let (com1fixings, com1contracts, com1weights)  = getFixings refMonth com lags slope
            let (pastDetails1, futureDetails1 ) = splitDetails com1fixings com1weights com1contracts 
            let (f1, fw1, t1, v1 ) = getFutureInputs futureDetails1 getPrices1 getVol expDate
            let ( pw1, p1 ) = getPastInputs pastDetails1 (fun _ c -> getPrices1 c ) //ignore fixing date and return contract price from jcc curve
            (f1,fw1,t1,v1,pw1,p1)

        ///spread option of inst1 (e.g DBRT ) vs inst2 (e.g. JKM) 
        let optPricer inst1 inst2 rho refMonth = 
            //let rho = 0.4
            //let refMonth = "Dec20"
            //let freight = if refMonth = "JUL-21" then 0.85 else 1.0
            let (f1,fw1,t1,v1,pw1,p1) = getInputs expDate refMonth lags1 inst1 slope pricecurve1 volcurve1 price1 vol1
            let (f2,fw2,t2,v2,pw2,p2) = getInputs expDate refMonth lags2 inst2 1.0M pricecurve2 volcurve2 price2 vol2
            // put optionality:
            // exercise when JCC + freight < JKM, => -freight - ( JCC - JKM) > 0
            // so a put on ( JCC - JKM ) with strike  = -freight 
            let k = -freight
            //let opts = spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
            ////printfn "%A %A %A %A %A %A %A %A %f" f1 fw1 t1 v1 f2 fw2 t2 v2 rho
            let opt, deltas = optionChoi f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
            let p1 = ((f1 .* fw1 ).Sum() + freight) //inst1 forwd
            let p2 = ((f2 .* fw2 ).Sum()) //inst2 fwd
            let pintr = 
                match callput with 
                | Call -> (max (p1 - p2) 0.)
                | Put -> (max (p2 - p1) 0.)

            let deltaA = deltas
            [|   "Option", opt;
                "Delta1", deltaA.[0];
                "Delta2", deltaA.[1];
                "P1", p1;
                "P2", p2;
                "Intrinsic", pintr;
                "vol1", ( Statistics.Mean v1); //vol1 
                "vol2", ( Statistics.Mean v2); //vol1 
            |]

        optPricer inst1 inst2 rho refMonth
