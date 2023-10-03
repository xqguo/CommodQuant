namespace Commod
[<AutoOpen>]
module Pricer =
    open System
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Statistics
    open MathNet.Numerics.Differentiation

    let SwapPricer inst d1 d2 (f:PriceCurve) pd = //std swap pricer 
        let c = getCommod inst
        let s = getSwap inst d1 d2 ( c.LotSize ) ( c.Quotation * 0.M) 
        let a = priceSwap s f pd 
        a.Value / s.Quantity.Value  |> float

    // get equal weights based on the number of fixings
    let getEqualWeights x =
        let n = Array.length x
        let w = 1.0 / float n 
        Array.replicate n w 

    let inline toVector s = s |> Array.map float |> vector 

    /// split fixings into future and past using pricingDate
    let splitDetails pricingDate details = 
        let n = details |> Array.tryFindIndex( fun (x,_,_) -> x > pricingDate ) 
        match n with
        | Some 0 -> (Array.empty, details)
        | Some i -> Array.splitAt i details
        | None -> (details, Array.empty)

    let shiftMonth refMonth l =
        let refDate = refMonth |> pillarToDate 
        refDate.AddMonths l |> formatPillar
        
    ///take refmonth and return array of tuple: 
    ///fixingdate, weight, contract, 
    ///slope is applied here into the weights.
    let getFixings refMonth (com:Commod) lags slope avg expDate =     
        let refDate = refMonth |> pillarToDate 
        //get reference contract, swap for oil, bullet for gas
        let avgfwd = getAvgFwd com.Instrument
        let contracts' = getNrbyContracts avgfwd
        lags 
        |> Array.map( fun i -> 
            let refMonth = refDate.AddMonths i 
            let contract = refMonth |> formatPillar
            let d1,d2 = 
                match com.Instrument with 
                | JKM | TTF | NG -> //for gas, use the contract month
                    getContractMonth contracts' contract
                | _ ->   refMonth, dateAdjust' "e" refMonth
            //let dates = getFixingDates avgfwd.Frequency com.Calendar d1 d2 
            let dates = getFixingDates avg com.Calendar d1 d2 
            //let contracts = List.replicate dates.Length contract
            let contracts = getFixingContracts contracts' dates
            let weights = (getEqualWeights dates) |> Array.map( fun x -> x /(float lags.Length) * (float slope))
            let fixingDates = dates |> Array.map( fun d -> min d expDate)
            fixingDates, weights, contracts
            )
        |> Array.reduce( fun ( d1,w1,c1) (d2,w2,c2) -> 
            (Array.append d1 d2), 
            (Array.append w1 w2),
            (Array.append c1 c2))
        //consolidate future details to group weightes for same fixing dates and same contracts
        |||> Array.zip3
        |> Array.groupBy(fun (x,_,z) -> x,z) 
        |> Array.map( fun ((k1,k2),v) -> 
            k1,(v |> Array.sumBy( fun (_,x,_)->x)),k2)

    ///take array of weights and lags as well as the necessary arguments to 
    ///call getFixings with each refMonth with lag applied
    ///Finally group the results 
    let getFixingsWeighted refMonth (com:Commod) lags slope avg expDate weights=     
        weights
        |> Array.map( fun (w,l) ->
            let m = shiftMonth refMonth l
            let s = slope * decimal w
            getFixings m com lags s avg expDate
        )
        |> Array.collect id
        |> Array.groupBy(fun (x,_,z) -> x,z) 
        |> Array.map( fun ((k1,k2),v) -> 
            k1,(v |> Array.sumBy( fun (_,x,_)->x)),k2)

    //https://www.argusmedia.com/-/media/Files/methodology/argus-lng-daily.ashx
    //The construction of the oil-price average is expressed as three figures,
    //for example 601, representing the number of months over which the oil
    //price is averaged, the delay, or lag, in months between the end of the
    //oil price average period and the delivery period for the LNG, and the
    //number of months of delivery for which the average oil price pertains.
    //Argus produces prices for
    //• 601 – six-month average, no lag, for one month of delivery
    //• 301 – three-month average, no lag, for one month of delivery
    //• 311 – three-month average, one-month lag, for one month of
    //delivery
    //• 101 – one-month average, no lag, for one month of delivery
    //• 603 – six-month average, no lag, for one calendar quarter of delivery
    let applyFormula (f:string) (refMonth:DateTime) =
        //check formula: 3 digits
        if f.Length <> 3 || f.ToCharArray() |> Array.forall( Char.IsDigit ) |> not
            then failwith $"Unknown formula {f}"
        //if delivery is 3 or 6, move to calendar period start
        let m = 
            match f.[2] with
            | '6' -> refMonth |> dateAdjust' "H" 
            | '3' -> refMonth |> dateAdjust' "Q"
            | '1' -> refMonth |> dateAdjust' "a"
            | _ -> failwith $"Unknown formula {f}"
        //compute total lag
        let l =
            match (string f.[1])  with
            | Int i -> m.Month - refMonth.Month - i
            | _ -> failwith $"Unknown formula {f}"
        //apply avg
        match f.[0] with
        | '6' -> [|-6 .. -1|] 
        | '3' -> [|-3 .. -1|]
        | '1' -> [|-1|]
        | _ -> failwith $"Unknown formula {f}"
        |> Array.map ((+) l)

    ///generate inputs for option pricing 
    /// inputs are
    /// futureDetails is a list of tuple of fixingdate, weight, ContractPillar 
    /// getPriceFunc take contractPillar and return price 
    /// getVolFunc take contractPillar and return vol
    /// return tuple of 4 vectors:
    /// forwards, weights, time to maturity, vols
    let getFutureInputs futureDetails getPriceFunc getVolFunc =
        //consolidate future details to group weightes for same fixing dates and same contracts
        let (t1,fw1,contracts) = futureDetails |> Array.unzip3 
        let f1 = contracts |> Array.map getPriceFunc 
        let v1 = contracts |> Array.map getVolFunc 
        (f1, fw1, t1, v1)

    ///generate past average required for asian option
    let getPastInputs pastDetails getFixingFunc = 
        pastDetails |> Array.map(fun ( d, w, c ) -> float (getFixingFunc d c) * w ) |> Array.sum

    //for the final portfolio we need just functions that take price curve and return price. 
    let getInputs pricingDate expDate refMonth lags avg inst slope (pricecurve:PriceCurve) (volcurve:VolCurve) = 
        let com = getCommod inst
        let getPrices1 c = 
                if pricecurve.Pillars.Contains c then
                    (pricecurve.Item c).Value
                else
                    failwithf "try to getPrice:%s from %A" c pricecurve
        let getVol c = 
                if volcurve.Pillars.Contains c then
                    volcurve.Item c
                else
                    failwithf "try to get vol:%s from %A" c volcurve

        let (pastDetails1, futureDetails1 ) = splitDetails pricingDate ( getFixings refMonth com lags slope avg expDate )
        let (f1, fw1, d1, v1 ) = getFutureInputs futureDetails1 getPrices1 getVol 
        let p1 = getPastInputs pastDetails1 (fun d c -> 
            match (getfixing inst d ) with
            | Some v -> v 
            | None -> getPrices1 c )
        let t1 = d1 |> Array.map (getTTM pricingDate)
        ( toVector f1,
          toVector fw1,
          toVector t1,
          toVector v1,
          p1)


    ///spread option of inst1 (e.g DBRT ) vs inst2 (e.g. JKM) 
    let SpreadOptionPricerBS inst1 lags1 avg1 inst2 lags2 avg2 slope freight callput expDate  
        refMonth (pricingDate:DateTime)
        rho pricecurve1 volcurve1 pricecurve2 volcurve2 o =
        let optPricer inst1 inst2 rho refMonth = 
            //let rho = 0.4
            //let refMonth = "Dec20"
            //let freight = if refMonth = "JUL-21" then 0.85 else 1.0
            let (f1,fw1,t1,v1,a1) = getInputs pricingDate expDate refMonth lags1 avg1 inst1 slope pricecurve1 volcurve1 
            let (f2,fw2,t2,v2,a2) = getInputs pricingDate expDate refMonth lags2 avg2 inst2 1.0M pricecurve2 volcurve2 
            // put optionality:
            // exercise when JCC + freight < JKM, => -freight - ( JCC - JKM) > 0
            // so a put on ( JCC - JKM ) with strike  = -freight 
            let k = -freight - a1 + a2 /// adapte K for past fixings
            //let opts = spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
            ////printfn "%A %A %A %A %A %A %A %A %f" f1 fw1 t1 v1 f2 fw2 t2 v2 rho
            let opt, deltas =  optionChoi2AssetN f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput o
            let p1 = ((f1 .* fw1 ).Sum() + freight) + a1  //inst1 forwd
            let p2 = ((f2 .* fw2 ).Sum())+ a2 //inst2 fwd
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
                "Extrinsic", opt - pintr;
                "vol1", ( Statistics.Mean v1); //vol1 
                "vol2", ( Statistics.Mean v2); //vol1 
            |]

        optPricer inst1 inst2 rho refMonth

    let getPricesWithOverride (crv:PriceCurve) p c = 
       match p with
       | Some v -> v
       | None ->
            if crv.Pillars.Contains c then
                (crv.Item c).Value
            else
                failwithf "try to getPrice:%s from %A" c crv

    ///spread option using Gabillon model
    let SpreadOptionPricerGabillon inst1 lags1 avg1 inst2 lags2 avg2 slope freight callput expDate  
        refMonth (pricingDate:DateTime)
        rho pricecurve1 volcurve1 pricecurve2 volcurve2 =

        let getInputsG pricingDate expDate refMonth lags1 avg1 inst1 slope (pricecurve1:PriceCurve) volcurve1 = 
            let com1 = getCommod inst1
            let getPrices1 = getPricesWithOverride pricecurve1 None
            let (pastDetails1, futureDetails1 ) = splitDetails pricingDate ( getFixings refMonth com1 lags1 slope avg1 expDate )           
            let fixings1 = futureDetails1 |> Array.map( fun (x,_,y) -> (min x expDate),y)
            let fw1 = futureDetails1 |> Array.map( fun (_,w,_) -> w) |> toVector
            let sigma1 = getGabillonCov inst1 volcurve1 (getGabillonParam inst1) fixings1 pricingDate 
            let t1 = fixings1 |> Array.unzip |> fst |> Array.map (getTTM pricingDate) |> toVector
            let f1 = fixings1 |> Array.map( fun (_,c) -> getPrices1 c) |> toVector
            //let a1 = getPastInputs pastDetails1 (fun _ c -> getPrices1 c )
            let a1 = getPastInputs pastDetails1 (fun d c -> 
                match (getfixing inst1 d ) with
                | Some v -> v 
                | None -> getPrices1 c )
            (f1,fw1,t1,sigma1,a1)

        let (f1,fw1,t1,v1,a1) = getInputsG pricingDate expDate refMonth lags1 avg1 inst1 slope pricecurve1 volcurve1
        let (f2,fw2,t2,v2,a2) = getInputsG pricingDate expDate refMonth lags2 avg2 inst2 1.0M pricecurve2 volcurve2
        let k = -freight - a1 + a2 /// adapte K for past fixings
        //let opt, deltas =  optionChoi2AssetCov f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput //cov breakdown too often
        let v1' = ( v1.Diagonal() ./ t1 ).PointwiseSqrt()
        let v2' = ( v2.Diagonal() ./ t2 ).PointwiseSqrt()
        let opt, deltas =  optionChoi2AssetN f1 fw1 t1 v1' f2 fw2 t2 v2' k rho callput [17;2]
        let p1 = ((f1 .* fw1 ).Sum() + freight) + a1  //inst1 forwd
        let p2 = ((f2 .* fw2 ).Sum())+ a2 //inst2 fwd
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
            "Extrinsic", opt - pintr;
            "vol1", ( Statistics.Mean (v1.Diagonal() ./ t1 |> Vector.Sqrt ) ); //vol1 
            "vol2", ( Statistics.Mean (v2.Diagonal() ./ t2 |> Vector.Sqrt ) ); //vol1 
        |]

    let getInputsGWeighted pricingDate expDate refMonth lags1 avg1 inst1 slope (pricecurve1:PriceCurve) weights = 
            let com1 = getCommod inst1
            let getPrices1 = getPricesWithOverride pricecurve1 None 
            let (pastDetails1, futureDetails1 ) = splitDetails pricingDate ( getFixingsWeighted refMonth com1 lags1 slope avg1 expDate weights )           
            let fixings1 = futureDetails1 |> Array.map( fun (x,_,y) -> (min x expDate),y)
            let fw1 = futureDetails1 |> Array.map( fun (_,w,_) -> w) |> toVector
            let f1 = fixings1 |> Array.map( fun (_,c) -> getPrices1 c) |> toVector
            //let a1 = getPastInputs pastDetails1 (fun _ c -> getPrices1 c )
            let a1 = getPastInputs pastDetails1 (fun d c -> 
                match (getfixing inst1 d ) with
                | Some v -> v 
                | None -> getPrices1 c )
            (f1,fw1,fixings1,a1)

    let getInputsG pricingDate expDate refMonth lags1 avg1 inst1 slope (pricecurve1:PriceCurve) = 
            let weights = [|(1.0,0)|] // default weight 
            getInputsGWeighted pricingDate expDate refMonth lags1 avg1 inst1 slope pricecurve1 weights 
 
    ///spread option using cross Gabillon model
    let SpreadOptionPricerXGabillonWeighted inst1 lags1 avg1 inst2 lags2 avg2 slope freight callput expDate  
        refMonth (pricingDate:DateTime)
        xParam pricecurve1 volcurve1 pricecurve2 volcurve2 o weights=
        let (f1,fw1,x1,a1) = getInputsGWeighted pricingDate expDate refMonth lags1 avg1 inst1 slope pricecurve1 weights
        let (f2,fw2,x2,a2) = getInputsGWeighted pricingDate expDate refMonth lags2 avg2 inst2 1.0M pricecurve2 weights
        let k = -freight - a1 + a2 /// adapte K for past fixings
        //let opt, deltas =  optionChoi2AssetCov f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput //cov breakdown too often
        //let xParam = getXGabillonParam inst1 inst2 rho 
        let sigma = getXGabillonCovFull inst1 volcurve1 x1 inst2 volcurve2 x2 xParam pricingDate 
        let t1 = x1 |> Array.map (fst >> getTTM pricingDate ) |> toVector
        let t2 = x2 |> Array.map (fst >> getTTM pricingDate) |> toVector
        let n = f1.Count
        let opt, deltas =  optionChoi2AssetCov f1 fw1 f2 fw2 k sigma callput o
        let p1 = ((f1 .* fw1 ).Sum() + freight) + a1  //inst1 forwd
        let p2 = ((f2 .* fw2 ).Sum())+ a2 //inst2 fwd
        let pintr = 
            match callput with 
            | Call -> (max (p1 - p2) 0.)
            | Put -> (max (p2 - p1) 0.)

        let v1 = ( sigma.Diagonal().[0..n-1] ./ t1 ).PointwiseSqrt()
        let v2 = ( sigma.Diagonal().[n..] ./ t2 ).PointwiseSqrt()
        let deltaA = deltas
        [|  "Option", opt;
            "Delta1", deltaA.[0];
            "Delta2", deltaA.[1];
            "P1", p1;
            "P2", p2;
            "Intrinsic", pintr;
            "Extrinsic", opt - pintr;
            "vol1", ( Statistics.Mean v1 ); //vol1 
            "vol2", ( Statistics.Mean v2 ); //vol1 
        |]
    let SpreadOptionPricerXGabillon inst1 lags1 avg1 inst2 lags2 avg2 slope freight callput expDate  
        refMonth (pricingDate:DateTime)
        xParam pricecurve1 volcurve1 pricecurve2 volcurve2 o =
        SpreadOptionPricerXGabillonWeighted inst1 lags1 avg1 inst2 lags2 avg2 slope freight callput expDate  
            refMonth (pricingDate:DateTime)
            xParam pricecurve1 volcurve1 pricecurve2 volcurve2 o [|(1.0,0)|]
 
  ///asian and swaption pricer using Gabillon model
    let AsianOptionPricerGabillon inst lags avg k callput expDate  
        refMonth (pricingDate:DateTime)
        gParam pricecurve volcurve =
        let (f1,fw1,x1,a1) = getInputsG pricingDate expDate refMonth lags avg inst 1.0M pricecurve 
        let k' =  k - a1 // adapte K for past fixings
        let sigma = getGabillonCov inst volcurve gParam x1 pricingDate
        let t1 = x1 |> Array.map (fst >> getTTM pricingDate ) |> toVector
        let opt, delta =  asianoptionChoi f1 fw1 k' sigma callput
        let h = NumericalDerivative()
        let g = 
            ( fun ( d:float[]) -> 
                let f1'' = f1 + d.[0]
                let pd = pricingDate.AddDays d.[2]
                let v' = volcurve.shift (decimal d.[1])
                let s = getGabillonCov inst v' gParam x1 pd
                let opt, _ =  asianoptionChoi f1'' fw1 k' s callput
                opt)
        let d = [|0.0;0.0;0.0|]
        let gamma = h.EvaluatePartialDerivative( g, d, 0, 2 ) //gamma
        let vega = h.EvaluatePartialDerivative( g, d, 1, 1 ) / 100. //vega 1 vol
        let theta = h.EvaluatePartialDerivative( g, d, 2, 1 )//1d theta
        let p1 = (f1 .* fw1 ).Sum() + a1  //inst1 forwd
        let pintr = 
            match callput with 
            | Call -> (max (p1 - k) 0.)
            | Put -> (max (k - p1) 0.)
        let v1 = ( sigma.Diagonal()./ t1 ).PointwiseSqrt().Mean()
        [|  "Option", opt;
            "Delta1", delta;
            "Gamma1", gamma;
            "Vega1", vega;
            "Theta1", theta;
            "P1", p1;
            "Intrinsic", pintr;
            "Extrinsic", opt - pintr;
            "vol1", v1
        |]

    ///asian and swaption pricer using moment matching model
    let AsianOptionPricer inst lags avg k callput expDate  
        refMonth (pricingDate:DateTime) pricecurve volcurve =
        let (f1,fw1,t1,v1,a1) = getInputs pricingDate expDate refMonth lags avg inst 1.0M pricecurve volcurve 
        let opt, delta =  asianOptionAndDelta f1 fw1 t1 v1 k callput a1
        let p1 = (f1 .* fw1 ).Sum() + a1  //inst1 forwd
        let pintr = 
            match callput with 
            | Call -> (max (p1 - k) 0.)
            | Put -> (max (k - p1) 0.)
        [|  "Option", opt;
            "Delta1", delta;
            "P1", p1;
            "Intrinsic", pintr;
            "Extrinsic", opt - pintr;
            "vol1", v1.Mean()
        |]

    ///asian and swaption pricer using moment matching model
    ///with no past fixings, seems to be what ICE settlement is for.
    ///This is used for implied asian vol from listed apo settlements. 
    let AsianOptionPricer' inst lags avg k callput expDate  
        refMonth (pricingDate:DateTime) pricecurve volcurve =
        let getFixings refMonth (com:Commod) lags slope avg expDate =     
            let refDate = refMonth |> pillarToDate 
            //get reference contract, swap for oil, bullet for gas
            let avgfwd = getAvgFwd com.Instrument
            let contracts' = getNrbyContracts avgfwd
            lags 
            |> Array.map( fun i -> 
                let refMonth = refDate.AddMonths i 
                let contract = refMonth |> formatPillar
                let d1,d2 = 
                    match com.Instrument with 
                    | JKM | TTF | NG -> //for gas, use the contract month
                        getContractMonth contracts' contract
                    | _ ->   refMonth, dateAdjust' "e" refMonth
                //let dates = getFixingDates avgfwd.Frequency com.Calendar d1 d2 
                let dates = getFixingDates avg com.Calendar (max d1 (pricingDate.AddDays(1.0)) ) d2 
                //let contracts = List.replicate dates.Length contract
                let contracts = getFixingContracts contracts' dates
                let weights = (getEqualWeights dates) |> Array.map( fun x -> x /(float lags.Length) * (float slope))
                let fixingDates = dates |> Array.map( fun d -> min d expDate)
                fixingDates, weights, contracts
                )
            |> Array.reduce( fun ( d1,w1,c1) (d2,w2,c2) -> 
                (Array.append d1 d2), 
                (Array.append w1 w2),
                (Array.append c1 c2))
            //consolidate future details to group weightes for same fixing dates and same contracts
            |||> Array.zip3
            |> Array.groupBy(fun (x,_,z) -> x,z) |> Array.map( fun ((k1,k2),v) -> k1,(v |> Array.sumBy( fun (_,x,_)->x)),k2)
        let getInputs pricingDate expDate refMonth lags avg inst slope (pricecurve:PriceCurve) (volcurve:VolCurve) = 
            let com = getCommod inst
            let getPrices1 c = 
                    if pricecurve.Pillars.Contains c then
                        (pricecurve.Item c).Value
                    else
                        failwithf "try to getPrice:%s from %A" c pricecurve
            let getVol c = 
                    if volcurve.Pillars.Contains c then
                        volcurve.Item c
                    else
                        failwithf "try to get vol:%s from %A" c volcurve

            let (pastDetails1, futureDetails1 ) = splitDetails pricingDate ( getFixings refMonth com lags slope avg expDate )
            let (f1, fw1, d1, v1 ) = getFutureInputs futureDetails1 getPrices1 getVol 
            //let p1 = getPastInputs pastDetails1 (fun _ c -> getPrices1 c ) 
            let p1 = getPastInputs pastDetails1 (fun d c -> 
                match (getfixing inst d ) with
                | Some v -> v 
                | None -> getPrices1 c )
            let t1 = d1 |> Array.map (getTTM pricingDate)
            ( toVector f1,
              toVector fw1,
              toVector t1,
              toVector v1,
              p1)
        let (f1,fw1,t1,v1,a1) = getInputs pricingDate expDate refMonth lags avg inst 1.0M pricecurve volcurve 
        let opt =  asianoption f1 fw1 t1 v1 k callput 0.
        [|  "Option", opt |]

    let AsianOptionPricerSmile inst lags avg k callput expDate  
        refMonth (pricingDate:DateTime) pricecurve (smile:VolDeltaSmile) =
        let (f1,fw1,x1,a1) = getInputsG pricingDate expDate refMonth lags avg inst 1.0M pricecurve
        let m = x1.Length / 2 //choose middle fixing
        let t',c' = x1.[m]
        let f = f1 * fw1 
        let k' = k - a1 
        let volcurve = getRefDelta f k' t' c' smile pricingDate
        let getVol c = 
            if volcurve.Pillars.Contains c then
                volcurve.Item c
            else
                failwithf "try to get vol:%s from %A" c volcurve
        let v1 = x1 |> Array.map (snd >> getVol ) |> toVector
        let h = NumericalDerivative()
        let g = 
            ( fun ( d:float[]) -> 
                let f1'' = f1 + d.[0]
                let smile' = smile.Shift d.[1]
                let pd = pricingDate.AddDays d.[2]
                let v' = getRefDelta f k' t' c' smile' pd
                let getVol' c = v'.Item c
                let v'' = x1 |> Array.map (snd >> getVol' ) |> toVector
                let t'' = x1 |> Array.map (fst >> getTTM pd ) |> toVector
                asianoption f1'' fw1 t'' v'' k callput a1 )
        let d = [|0.0;0.0;0.0|]
        let opt = h.EvaluatePartialDerivative( g, d, 0, 0 ) //opt
        let delta = h.EvaluatePartialDerivative( g, d, 0, 1 ) //delta
        let gamma = h.EvaluatePartialDerivative( g, d, 0, 2 ) //gamma
        let vega = h.EvaluatePartialDerivative( g, d, 1, 1 ) / 100. //vega 1 vol
        let theta = -h.EvaluatePartialDerivative( g, d, 2, 1 ) / 365.//1d theta
        let p1 = (f1 .* fw1 ).Sum() + a1  //inst1 forwd
        let pintr = 
            match callput with 
            | Call -> (max (p1 - k) 0.)
            | Put -> (max (k - p1) 0.)
        [|  "Option", opt;
            "Delta1", delta;
            "Gamma1", gamma;
            "Vega1", vega;
            "Theta1", theta;
            "P1", p1;
            "Intrinsic", pintr;
            "Extrinsic", opt - pintr;
            "vol1", v1.Mean()
        |]

    ///asian and swaption pricer using Gabillon model
    let AsianOptionPricerSmileGabillon inst lags avg k callput expDate  
        refMonth (pricingDate:DateTime)
        gParam pricecurve smile =
        let com = getCommod inst
        let (f1,fw1,x1,a1) = getInputsG pricingDate expDate refMonth lags avg inst 1.0M pricecurve 
        let m = x1.Length / 2 //choose middle fixing
        let t',c' = x1.[m]
        let fe = com.Contracts.Fut.[c'] 
        let oe = com.Contracts.Opt.[c'] 
        let f = f1 * fw1 
        let k' =  k - a1 // adapte K for past fixings
        let volcurve = getRefDeltaGabillon f k' t' c' smile oe fe gParam pricingDate
        let t1 = x1 |> Array.map (fst >> getTTM pricingDate ) |> toVector
        let sigma = getGabillonCov inst volcurve gParam x1 pricingDate
        let v1 = ( sigma.Diagonal()./ t1 ).PointwiseSqrt().Mean()
        let h = NumericalDerivative()
        let g = 
            ( fun ( d:float[]) -> 
                let f1'' = f1 + d.[0]
                let f'' = f1 * fw1 
                let s'' = smile.Shift d.[1] 
                let pd = pricingDate.AddDays d.[2]
                let volcurve = getRefDeltaGabillon f'' k' t' c' s'' oe fe gParam pd
                let sigma' = getGabillonCov inst volcurve gParam x1 pd
                let opt, _ =  asianoptionChoi f1'' fw1 k' sigma' callput
                opt)
        let d = [|0.0;0.0;0.0|]
        let opt = h.EvaluatePartialDerivative( g, d, 0, 0 ) //opt
        let delta = h.EvaluatePartialDerivative( g, d, 0, 1 ) //delta
        let gamma = h.EvaluatePartialDerivative( g, d, 0, 2 ) //gamma
        let vega = h.EvaluatePartialDerivative( g, d, 1, 1 ) / 100. //vega 1 vol
        let theta = h.EvaluatePartialDerivative( g, d, 2, 1 ) //1d theta
        let p1 = f + a1  //inst1 forwd
        let pintr = 
            match callput with 
            | Call -> (max (p1 - k) 0.)
            | Put -> (max (k - p1) 0.)
        [|  "Option", opt;
            "Delta1", delta;
            "Gamma1", gamma;
            "Vega1", vega;
            "Theta1", theta;
            "P1", p1;
            "Intrinsic", pintr;
            "Extrinsic", opt - pintr;
            "vol1", v1
        |]

