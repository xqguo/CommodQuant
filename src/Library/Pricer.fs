namespace Commod

[<AutoOpen>]
module Pricer =
    open System
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Statistics
    open MathNet.Numerics.Differentiation

    /// <summary>
    /// Calculates the standard swap price for a given instrument and period.
    /// </summary>
    /// <param name="inst">The instrument for which to price the swap.</param>
    /// <param name="d1">The start date of the swap period.</param>
    /// <param name="d2">The end date of the swap period.</param>
    /// <param name="f">The price curve function.</param>
    /// <param name="pd">The pricing date.</param>
    /// <returns>The calculated swap price as a float.</returns>
    let SwapPricer inst d1 d2 (f: PriceCurve) pd = //std swap pricer
        let c = getCommod inst
        let s = getSwap inst d1 d2 (c.LotSize) (c.Quotation * 0.M)
        let a = priceSwap s f pd
        a.Value / s.Quantity.Value |> float

    // get equal weights based on the number of fixings
    /// <summary>
    /// Calculates equal weights based on the number of items in an array.
    /// </summary>
    /// <param name="x">The input array.</param>
    /// <returns>An array of floats representing equal weights.</returns>
    let getEqualWeights x =
        let n = Array.length x
        let w = 1.0 / float n
        Array.replicate n w

    /// <summary>
    /// Converts an array of numbers (typically floats or values convertible to float) into a MathNet dense vector of floats.
    /// </summary>
    /// <param name="s">The input array to convert.</param>
    /// <returns>A MathNet dense vector of floats.</returns>
    let inline toVector s = s |> Array.map float |> vector

    /// <summary>
    /// Splits an array of details (tuples containing a date) into two arrays: one with past details and one with future details, based on a pricing date.
    /// </summary>
    /// <param name="pricingDate">The date used to split the details. Details on or before this date are considered past.</param>
    /// <param name="details">An array of tuples, where the first element of the tuple is a DateTime representing the date of the detail.</param>
    /// <returns>A tuple containing two arrays: the first array holds details on or before the pricingDate (past), and the second array holds details after the pricingDate (future).</returns>
    /// split fixings into future and past using pricingDate
    let splitDetails pricingDate details =
        let n = details |> Array.tryFindIndex (fun (x, _, _) -> x > pricingDate)

        match n with
        | Some 0 -> (Array.empty, details)
        | Some i -> Array.splitAt i details
        | None -> (details, Array.empty)

    /// <summary>
    /// Shifts a reference month by a specified number of months and returns the resulting contract pillar string.
    /// </summary>
    /// <param name="refMonth">The reference contract pillar (string) to shift, e.g., "JAN-25".</param>
    /// <param name="l">The number of months to shift the reference month by (integer).</param>
    /// <returns>The shifted contract pillar as a string.</returns>
    let shiftMonth refMonth l =
        let refDate = refMonth |> pillarToDate
        refDate.AddMonths l |> formatPillar

    /// <summary>
    /// Generates an array of fixing details (date, weight, contract) based on a reference month, commodity information, lags, slope, averaging rule, and expiration date.
    /// The slope is applied to the weights.
    /// </summary>
    /// <param name="refMonth">The reference contract pillar string (e.g., "JAN-25").</param>
    /// <param name="com">The commodity object containing instrument details.</param>
    /// <param name="lags">An array of integers representing the month lags to apply to the reference month.</param>
    /// <param name="slope">A decimal value representing the slope to be applied to the weights.</param>
    /// <param name="avg">A boolean indicating whether to use an averaging rule for fixing dates (e.g. average of month for JKM).</param>
    /// <param name="expDate">The expiration date, used to cap fixing dates if they fall beyond it.</param>
    /// <returns>An array of tuples, where each tuple contains a fixing date (DateTime), a weight (float), and a contract pillar string. Weights are grouped by identical fixing date and contract.</returns>
    ///take refmonth and return array of tuple:
    ///fixingdate, weight, contract,
    ///slope is applied here into the weights.
    let getFixings refMonth (com: Commod) lags slope avg expDate =
        let refDate = refMonth |> pillarToDate
        //get reference contract, swap for oil, bullet for gas
        let avgfwd = getAvgFwd com.Instrument
        let contracts' = getNrbyContracts avgfwd

        lags
        |> Array.map (fun i ->
            let refMonth = refDate.AddMonths i
            let contract = refMonth |> formatPillar

            let d1, d2 =
                match com.Instrument with
                | JKM
                | TTF
                | NBP
                | NG -> //for gas, use the contract month
                    getContractMonth contracts' contract
                | _ -> refMonth, dateAdjust' "e" refMonth
            //let dates = getFixingDates avgfwd.Frequency com.Calendar d1 d2
            let dates = getFixingDates avg com.Calendar d1 d2
            //let contracts = List.replicate dates.Length contract
            let contracts = getFixingContracts contracts' dates

            let weights =
                (getEqualWeights dates)
                |> Array.map (fun x -> x / (float lags.Length) * (float slope))

            let fixingDates = dates |> Array.map (fun d -> min d expDate)
            (fixingDates, weights, contracts) |||> Array.zip3)
        |> Array.concat
        // |> Array.reduce( fun ( d1,w1,c1) (d2,w2,c2) ->
        //     (Array.append d1 d2),
        //     (Array.append w1 w2),
        //     (Array.append c1 c2))
        // //consolidate future details to group weightes for same fixing dates and same contracts
        |> Array.groupBy (fun (x, _, z) -> x, z)
        |> Array.map (fun ((k1, k2), v) -> k1, (v |> Array.sumBy (fun (_, x, _) -> x)), k2)

    /// <summary>
    /// Generates fixing details similar to `getFixings`, but allows for applying an additional layer of weighting based on an input `weights` array.
    /// Each element in the `weights` array (a tuple of weight and lag) results in a call to `getFixings` for a shifted reference month, and the results are aggregated.
    /// </summary>
    /// <param name="refMonth">The base reference contract pillar string.</param>
    /// <param name="com">The commodity object.</param>
    /// <param name="lags">An array of month lags passed to `getFixings` for each weighted calculation.</param>
    /// <param name="slope">The base slope value, which gets further multiplied by the individual weight from the `weights` array.</param>
    /// <param name="avg">A boolean indicating whether to use an averaging rule for fixing dates.</param>
    /// <param name="expDate">The expiration date.</param>
    /// <param name="weights">An array of (float, int) tuples, where the float is a weighting factor and the int is an additional month lag for each call to `getFixings`.</param>
    /// <returns>An array of tuples (DateTime, float, string) representing aggregated and weighted fixing details, filtered to remove zero-weight entries.</returns>
    ///take array of weights and lags as well as the necessary arguments to
    ///call getFixings with each refMonth with lag applied
    ///Finally group the results
    let getFixingsWeighted refMonth (com: Commod) lags slope avg expDate weights =
        weights
        |> Array.map (fun (w, l) ->
            let m = shiftMonth refMonth l
            let s = slope * decimal w
            getFixings m com lags s avg expDate)
        |> Array.collect id
        |> Array.groupBy (fun (x, _, z) -> x, z)
        |> Array.map (fun ((k1, k2), v) -> k1, (v |> Array.sumBy (fun (_, x, _) -> x)), k2)
        |> Array.filter (fun (_, w, _) -> abs w > 1E-12) //ignore 0 weights

    /// <summary>
    /// Parses a 3-digit string formula (e.g., "601", "311") and a reference month to determine a set of month lags.
    /// The formula encodes averaging period, lag, and delivery period duration, commonly used in oil-linked LNG pricing.
    /// </summary>
    /// <param name="f">A 3-digit string representing the formula. E.g., "601" for a six-month average, no lag, one month delivery.</param>
    /// <param name="refMonth">The reference DateTime for the delivery period.</param>
    /// <returns>An array of integers representing the calculated month lags relative to the (potentially adjusted) reference month.
    /// For example, for "601" and refMonth "JAN-25", it would return lags for a 6-month average period ending before JAN-25.</returns>
    /// <remarks>
    /// The formula digits typically represent:
    /// - Digit 1: Number of months for averaging (e.g., '6' for six months).
    /// - Digit 2: Lag in months between the end of the averaging period and the start of the delivery period.
    /// - Digit 3: Number of months of delivery (e.g., '1' for one month, '3' for a quarter, '6' for a semester).
    /// The function adjusts the reference month to the start of the calendar period if delivery is for a quarter or semester.
    /// </remarks>
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
    let applyFormula (f: string) (refMonth: DateTime) =
        //check formula: 3 digits
        if f.Length <> 3 || f.ToCharArray() |> Array.forall (Char.IsDigit) |> not then
            failwith $"Unknown formula {f}"
        //if delivery is 3 or 6, move to calendar period start
        let m =
            match f.[2] with
            | '6' -> refMonth |> dateAdjust' "H"
            | '3' -> refMonth |> dateAdjust' "Q"
            | '1' -> refMonth |> dateAdjust' "a"
            | _ -> failwith $"Unknown formula {f}"
        //compute total lag
        let l =
            match (string f.[1]) with
            | Int i -> m.Month - refMonth.Month - i
            | _ -> failwith $"Unknown formula {f}"
        //apply avg
        match f.[0] with
        | '6' -> [| -6 .. -1 |]
        | '3' -> [| -3 .. -1 |]
        | '1' -> [| -1 |]
        | _ -> failwith $"Unknown formula {f}"
        |> Array.map ((+) l)

    /// <summary>
    /// Prepares inputs for option pricing based on future fixing details.
    /// It takes future fixing details and functions to retrieve prices and volatilities, then returns these as arrays.
    /// </summary>
    /// <param name="futureDetails">An array of tuples, each containing a fixing date (DateTime), a weight (float), and a contract pillar string.</param>
    /// <param name="getPriceFunc">A function that takes a contract pillar string and returns its price (float).</param>
    /// <param name="getVolFunc">A function that takes a contract pillar string and returns its volatility (float).</param>
    /// <returns>A tuple of four arrays:
    /// 1. `f1`: Array of forward prices (float[]).
    /// 2. `fw1`: Array of weights (float[]).
    /// 3. `t1`: Array of fixing dates (DateTime[]).
    /// 4. `v1`: Array of volatilities (float[]).
    /// </returns>
    ///generate inputs for option pricing
    /// inputs are
    /// futureDetails is a list of tuple of fixingdate, weight, ContractPillar
    /// getPriceFunc take contractPillar and return price
    /// getVolFunc take contractPillar and return vol
    /// return tuple of 4 vectors:
    /// forwards, weights, time to maturity, vols
    let getFutureInputs futureDetails getPriceFunc getVolFunc =
        //consolidate future details to group weightes for same fixing dates and same contracts
        let (t1, fw1, contracts) = futureDetails |> Array.unzip3
        let f1 = contracts |> Array.map getPriceFunc
        let v1 = contracts |> Array.map getVolFunc
        (f1, fw1, t1, v1)

    /// <summary>
    /// Calculates the weighted average of past fixings.
    /// </summary>
    /// <param name="pastDetails">An array of tuples, where each tuple contains a past fixing date (DateTime), its weight (float), and the contract pillar string.</param>
    /// <param name="getFixingFunc">A function that takes a DateTime (fixing date) and a contract pillar string, and returns the fixing value (float) for that date and contract.</param>
    /// <returns>The sum of (fixing value * weight) for all past details, as a float.</returns>
    ///generate past average required for asian option
    let getPastInputs pastDetails getFixingFunc =
        pastDetails
        |> Array.map (fun (d, w, c) -> float (getFixingFunc d c) * w)
        |> Array.sum

    /// <summary>
    /// Gathers all necessary inputs for pricing models, including separating past and future fixings, and retrieving market data.
    /// </summary>
    /// <param name="pricingDate">The date for which pricing is being performed.</param>
    /// <param name="expDate">The expiration date of the instrument being priced.</param>
    /// <param name="refMonth">The reference month pillar string for the instrument.</param>
    /// <param name="lags">An array of integer month lags used by `getFixings`.</param>
    /// <param name="avg">A boolean indicating if averaging rules apply for `getFixings`.</param>
    /// <param name="inst">The instrument type/name.</param>
    /// <param name="slope">The slope parameter for `getFixings`.</param>
    /// <param name="pricecurve">The PriceCurve object to fetch prices if not fixed.</param>
    /// <param name="volcurve">The VolCurve object to fetch volatilities.</param>
    /// <returns>A tuple containing:
    /// 1. `f1Vec`: Vector of forward prices for future fixings.
    /// 2. `fw1Vec`: Vector of weights for future fixings.
    /// 3. `t1Vec`: Vector of times to maturity (in years) for future fixings.
    /// 4. `v1Vec`: Vector of volatilities for future fixings.
    /// 5. `p1`: The weighted average of past fixings.
    /// </returns>
    //for the final portfolio we need just functions that take price curve and return price.
    let getInputs pricingDate expDate refMonth lags avg inst slope (pricecurve: PriceCurve) (volcurve: VolCurve) =
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

        let (pastDetails1, futureDetails1) =
            splitDetails pricingDate (getFixings refMonth com lags slope avg expDate)

        let (f1, fw1, d1, v1) = getFutureInputs futureDetails1 getPrices1 getVol

        let p1 =
            getPastInputs pastDetails1 (fun d c ->
                match (getfixing inst d) with
                | Some v -> v
                | None -> getPrices1 c)

        let t1 = d1 |> Array.map (getTTM pricingDate)
        (toVector f1, toVector fw1, toVector t1, toVector v1, p1)

    /// <summary>
    /// Prices a spread option using a Black-Scholes-like model (Choi's 2-asset correlation option).
    /// This function orchestrates input gathering via `getInputs` and then calls the core option pricing logic.
    /// </summary>
    /// <param name="inst1">Instrument 1 name/type.</param>
    /// <param name="lags1">Lags for instrument 1.</param>
    /// <param name="avg1">Averaging rule for instrument 1.</param>
    /// <param name="inst2">Instrument 2 name/type.</param>
    /// <param name="lags2">Lags for instrument 2.</param>
    /// <param name="avg2">Averaging rule for instrument 2.</param>
    /// <param name="slope">Slope parameter (typically for instrument 1's pricing formula).</param>
    /// <param name="freight">Freight cost, used in calculating the strike for the spread.</param>
    /// <param name="callput">Option type: Call or Put.</param>
    /// <param name="expDate">Expiration date of the option.</param>
    /// <param name="refMonth">Reference month for the option contracts.</param>
    /// <param name="pricingDate">The date as of which pricing is performed.</param>
    /// <param name="rho">Correlation between instrument 1 and instrument 2.</param>
    /// <param name="pricecurve1">Price curve for instrument 1.</param>
    /// <param name="volcurve1">Volatility curve for instrument 1.</param>
    /// <param name="pricecurve2">Price curve for instrument 2.</param>
    /// <param name="volcurve2">Volatility curve for instrument 2.</param>
    /// <param name="o">Integration order/parameter for `optionChoi2AssetN`.</param>
    /// <returns>An array of (string, float) tuples representing pricing results (e.g., "Option", "Delta1", "P1", "Intrinsic").</returns>
    ///spread option of inst1 (e.g DBRT ) vs inst2 (e.g. JKM)
    let SpreadOptionPricerBS
        inst1
        lags1
        avg1
        inst2
        lags2
        avg2
        slope
        freight
        callput
        expDate
        refMonth
        (pricingDate: DateTime)
        rho
        pricecurve1
        volcurve1
        pricecurve2
        volcurve2
        o
        =
        let optPricer inst1 inst2 rho refMonth =
            //let rho = 0.4
            //let refMonth = "Dec20"
            //let freight = if refMonth = "JUL-21" then 0.85 else 1.0
            let (f1, fw1, t1, v1, a1) =
                getInputs pricingDate expDate refMonth lags1 avg1 inst1 slope pricecurve1 volcurve1

            let (f2, fw2, t2, v2, a2) =
                getInputs pricingDate expDate refMonth lags2 avg2 inst2 1.0M pricecurve2 volcurve2
            // put optionality:
            // exercise when JCC + freight < JKM, => -freight - ( JCC - JKM) > 0
            // so a put on ( JCC - JKM ) with strike  = -freight
            let k = -freight - a1 + a2
            /// adapte K for past fixings
            //let opts = spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
            ////printfn "%A %A %A %A %A %A %A %A %f" f1 fw1 t1 v1 f2 fw2 t2 v2 rho
            let opt, deltas = optionChoi2AssetN f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput o
            let p1 = ((f1 .* fw1).Sum() + freight) + a1 //inst1 forwd
            let p2 = ((f2 .* fw2).Sum()) + a2 //inst2 fwd

            let pintr =
                match callput with
                | Call -> (max (p1 - p2) 0.)
                | Put -> (max (p2 - p1) 0.)

            let deltaA = deltas

            [| "Option", opt
               "Delta1", deltaA.[0]
               "Delta2", deltaA.[1]
               "P1", p1
               "P2", p2
               "Intrinsic", pintr
               "Extrinsic", opt - pintr
               "vol1", (Statistics.Mean v1) //vol1
               "vol2", (Statistics.Mean v2) |] //vol1

        optPricer inst1 inst2 rho refMonth

    /// <summary>
    /// Retrieves a price for a contract, prioritizing an optional override price.
    /// If the override price `p` is Some, its value is returned. Otherwise, the price is fetched from the `crv` (PriceCurve) for contract `c`.
    /// </summary>
    /// <param name="crv">The PriceCurve to use if no override price is provided.</param>
    /// <param name="p">An optional float representing an override price.</param>
    /// <param name="c">The contract pillar string for which to get the price.</param>
    /// <returns>The override price if available, otherwise the price from the curve. Raises an error if the contract is not on the curve and no override is given.</returns>
    let getPricesWithOverride (crv: PriceCurve) p c =
        match p with
        | Some v -> v
        | None ->
            if crv.Pillars.Contains c then
                (crv.Item c).Value
            else
                failwithf "try to getPrice:%s from %A" c crv

    /// <summary>
    /// Prices a spread option using the Gabillon model for Asian options, adapted for spreads.
    /// It uses `getInputsG` (Gabillon-specific input gathering) and `optionChoi2AssetN` (Choi's 2-asset numerical integration).
    /// </summary>
    /// <param name="inst1">Instrument 1 name/type.</param>
    /// <param name="lags1">Lags for instrument 1.</param>
    /// <param name="avg1">Averaging rule for instrument 1.</param>
    /// <param name="inst2">Instrument 2 name/type.</param>
    /// <param name="lags2">Lags for instrument 2.</param>
    /// <param name="avg2">Averaging rule for instrument 2.</param>
    /// <param name="slope">Slope parameter.</param>
    /// <param name="freight">Freight cost for strike calculation.</param>
    /// <param name="callput">Option type: Call or Put.</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month.</param>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="rho">Correlation between instruments.</param>
    /// <param name="pricecurve1">Price curve for instrument 1.</param>
    /// <param name="volcurve1">Volatility curve for instrument 1 (used for Gabillon covariance).</param>
    /// <param name="pricecurve2">Price curve for instrument 2.</param>
    /// <param name="volcurve2">Volatility curve for instrument 2 (used for Gabillon covariance).</param>
    /// <returns>An array of (string, float) tuples with pricing results.</returns>
    ///spread option using Gabillon model
    let SpreadOptionPricerGabillon
        inst1
        lags1
        avg1
        inst2
        lags2
        avg2
        slope
        freight
        callput
        expDate
        refMonth
        (pricingDate: DateTime)
        rho
        pricecurve1
        volcurve1
        pricecurve2
        volcurve2
        =

        let getInputsG pricingDate expDate refMonth lags1 avg1 inst1 slope (pricecurve1: PriceCurve) volcurve1 =
            let com1 = getCommod inst1
            let getPrices1 = getPricesWithOverride pricecurve1 None

            let (pastDetails1, futureDetails1) =
                splitDetails pricingDate (getFixings refMonth com1 lags1 slope avg1 expDate)

            let fixings1 = futureDetails1 |> Array.map (fun (x, _, y) -> (min x expDate), y)
            let fw1 = futureDetails1 |> Array.map (fun (_, w, _) -> w) |> toVector

            let sigma1 =
                getGabillonCov inst1 volcurve1 (getGabillonParam inst1) fixings1 pricingDate

            let t1 =
                fixings1 |> Array.unzip |> fst |> Array.map (getTTM pricingDate) |> toVector

            let f1 = fixings1 |> Array.map (fun (_, c) -> getPrices1 c) |> toVector
            //let a1 = getPastInputs pastDetails1 (fun _ c -> getPrices1 c )
            let a1 =
                getPastInputs pastDetails1 (fun d c ->
                    match (getfixing inst1 d) with
                    | Some v -> v
                    | None -> getPrices1 c)

            (f1, fw1, t1, sigma1, a1)

        let (f1, fw1, t1, v1, a1) =
            getInputsG pricingDate expDate refMonth lags1 avg1 inst1 slope pricecurve1 volcurve1

        let (f2, fw2, t2, v2, a2) =
            getInputsG pricingDate expDate refMonth lags2 avg2 inst2 1.0M pricecurve2 volcurve2

        let k = -freight - a1 + a2
        /// adapte K for past fixings
        //let opt, deltas =  optionChoi2AssetCov f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput //cov breakdown too often
        let v1' = (v1.Diagonal() ./ t1).PointwiseSqrt()
        let v2' = (v2.Diagonal() ./ t2).PointwiseSqrt()

        let opt, deltas =
            optionChoi2AssetN f1 fw1 t1 v1' f2 fw2 t2 v2' k rho callput [ 17; 2 ]

        let p1 = ((f1 .* fw1).Sum() + freight) + a1 //inst1 forwd
        let p2 = ((f2 .* fw2).Sum()) + a2 //inst2 fwd

        let pintr =
            match callput with
            | Call -> (max (p1 - p2) 0.)
            | Put -> (max (p2 - p1) 0.)

        let deltaA = deltas

        [| "Option", opt
           "Delta1", deltaA.[0]
           "Delta2", deltaA.[1]
           "P1", p1
           "P2", p2
           "Intrinsic", pintr
           "Extrinsic", opt - pintr
           "vol1", (Statistics.Mean(v1.Diagonal() ./ t1 |> Vector.Sqrt)) //vol1
           "vol2", (Statistics.Mean(v2.Diagonal() ./ t2 |> Vector.Sqrt)) |] //vol1

    /// <summary>
    /// Gathers inputs for Gabillon-style pricing, considering an additional layer of weighting.
    /// It's similar to `getInputs` but uses `getFixingsWeighted` and prepares data specifically for Gabillon models (e.g., not converting volatilities to vectors directly).
    /// </summary>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month pillar string.</param>
    /// <param name="lags1">Lags array for the instrument.</param>
    /// <param name="avg1">Averaging rule for the instrument.</param>
    /// <param name="inst1">Instrument name/type.</param>
    /// <param name="slope">Base slope parameter.</param>
    /// <param name="pricecurve1">Price curve for the instrument.</param>
    /// <param name="weights">An array of (float, int) tuples for weighted calculations, passed to `getFixingsWeighted`.</param>
    /// <returns>A tuple containing:
    /// 1. `f1`: Vector of forward prices.
    /// 2. `fw1`: Vector of weights.
    /// 3. `fixings1`: Array of (DateTime, string) tuples representing fixing dates and contract pillars.
    /// 4. `a1`: Weighted average of past fixings.
    /// </returns>
    let getInputsGWeighted pricingDate expDate refMonth lags1 avg1 inst1 slope (pricecurve1: PriceCurve) weights =
        let com1 = getCommod inst1
        let getPrices1 = getPricesWithOverride pricecurve1 None

        let (pastDetails1, futureDetails1) =
            splitDetails pricingDate (getFixingsWeighted refMonth com1 lags1 slope avg1 expDate weights)

        let fixings1 = futureDetails1 |> Array.map (fun (x, _, y) -> (min x expDate), y)
        let fw1 = futureDetails1 |> Array.map (fun (_, w, _) -> w) |> toVector
        let f1 = fixings1 |> Array.map (fun (_, c) -> getPrices1 c) |> toVector
        //let a1 = getPastInputs pastDetails1 (fun _ c -> getPrices1 c )
        let a1 =
            getPastInputs pastDetails1 (fun d c ->
                match (getfixing inst1 d) with
                | Some v -> v
                | None -> getPrices1 c)

        (f1, fw1, fixings1, a1)

    /// <summary>
    /// Gathers inputs for Gabillon-style pricing with a default weighting (single weight of 1.0, lag 0).
    /// This is a convenience wrapper around `getInputsGWeighted`.
    /// </summary>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month pillar string.</param>
    /// <param name="lags1">Lags array for the instrument.</param>
    /// <param name="avg1">Averaging rule for the instrument.</param>
    /// <param name="inst1">Instrument name/type.</param>
    /// <param name="slope">Slope parameter.</param>
    /// <param name="pricecurve1">Price curve for the instrument.</param>
    /// <returns>The same tuple structure as `getInputsGWeighted`.</returns>
    let getInputsG pricingDate expDate refMonth lags1 avg1 inst1 slope (pricecurve1: PriceCurve) =
        let weights = [| (1.0, 0) |] // default weight
        getInputsGWeighted pricingDate expDate refMonth lags1 avg1 inst1 slope pricecurve1 weights

    /// <summary>
    /// Prices a spread option using a cross-asset Gabillon model with weighted inputs.
    /// This model considers the covariance between the term structures of the two assets.
    /// </summary>
    /// <param name="inst1">Instrument 1.</param>
    /// <param name="lags1">Lags for instrument 1.</param>
    /// <param name="avg1">Averaging rule for instrument 1.</param>
    /// <param name="inst2">Instrument 2.</param>
    /// <param name="lags2">Lags for instrument 2.</param>
    /// <param name="avg2">Averaging rule for instrument 2.</param>
    /// <param name="slope">Slope for instrument 1's pricing formula.</param>
    /// <param name="freight">Freight cost for strike calculation.</param>
    /// <param name="callput">Option type (Call/Put).</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month.</param>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="xParam">Cross-Gabillon parameters for covariance calculation.</param>
    /// <param name="pricecurve1">Price curve for instrument 1.</param>
    /// <param name="volcurve1">Volatility curve for instrument 1.</param>
    /// <param name="pricecurve2">Price curve for instrument 2.</param>
    /// <param name="volcurve2">Volatility curve for instrument 2.</param>
    /// <param name="o">Integration order for `optionChoi2AssetCov`.</param>
    /// <param name="weights">Array of (weight, lag) tuples for `getInputsGWeighted`.</param>
    /// <returns>An array of (string, float) tuples with pricing results.</returns>
    ///spread option using cross Gabillon model
    let SpreadOptionPricerXGabillonWeighted
        inst1
        lags1
        avg1
        inst2
        lags2
        avg2
        slope
        freight
        callput
        expDate
        refMonth
        (pricingDate: DateTime)
        xParam
        pricecurve1
        volcurve1
        pricecurve2
        volcurve2
        o
        weights
        =
        let (f1, fw1, x1, a1) =
            getInputsGWeighted pricingDate expDate refMonth lags1 avg1 inst1 slope pricecurve1 weights

        let (f2, fw2, x2, a2) =
            getInputsGWeighted pricingDate expDate refMonth lags2 avg2 inst2 1.0M pricecurve2 weights

        let k = -freight - a1 + a2

        /// adapte K for past fixings
        //let opt, deltas =  optionChoi2AssetCov f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput //cov breakdown too often
        //let xParam = getXGabillonParam inst1 inst2 rho
        let sigma =
            getXGabillonCovFull inst1 volcurve1 x1 inst2 volcurve2 x2 xParam pricingDate

        let t1 = x1 |> Array.map (fst >> getTTM pricingDate) |> toVector
        let t2 = x2 |> Array.map (fst >> getTTM pricingDate) |> toVector
        let n = f1.Count
        let opt, deltas = optionChoi2AssetCov f1 fw1 f2 fw2 k sigma callput o
        let p1 = ((f1 .* fw1).Sum() + freight) + a1 //inst1 forwd
        let p2 = ((f2 .* fw2).Sum()) + a2 //inst2 fwd

        let pintr =
            match callput with
            | Call -> (max (p1 - p2) 0.)
            | Put -> (max (p2 - p1) 0.)

        let v1 = (sigma.Diagonal().[0 .. n - 1] ./ t1).PointwiseSqrt()
        let v2 = (sigma.Diagonal().[n..] ./ t2).PointwiseSqrt()
        let deltaA = deltas

        [| "Option", opt
           "Delta1", deltaA.[0]
           "Delta2", deltaA.[1]
           "P1", p1
           "P2", p2
           "Intrinsic", pintr
           "Extrinsic", opt - pintr
           "vol1", (Statistics.Mean v1) //vol1
           "vol2", (Statistics.Mean v2) |] //vol1

    /// <summary>
    /// Prices a spread option using a cross-asset Gabillon model with default weights.
    /// Convenience wrapper for `SpreadOptionPricerXGabillonWeighted` with a single (1.0, 0) weight.
    /// </summary>
    /// <param name="inst1">Instrument 1.</param>
    /// <param name="lags1">Lags for instrument 1.</param>
    /// <param name="avg1">Averaging rule for instrument 1.</param>
    /// <param name="inst2">Instrument 2.</param>
    /// <param name="lags2">Lags for instrument 2.</param>
    /// <param name="avg2">Averaging rule for instrument 2.</param>
    /// <param name="slope">Slope for instrument 1's pricing formula.</param>
    /// <param name="freight">Freight cost for strike calculation.</param>
    /// <param name="callput">Option type (Call/Put).</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month.</param>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="xParam">Cross-Gabillon parameters.</param>
    /// <param name="pricecurve1">Price curve for instrument 1.</param>
    /// <param name="volcurve1">Volatility curve for instrument 1.</param>
    /// <param name="pricecurve2">Price curve for instrument 2.</param>
    /// <param name="volcurve2">Volatility curve for instrument 2.</param>
    /// <param name="o">Integration order for `optionChoi2AssetCov`.</param>
    /// <returns>An array of (string, float) tuples with pricing results.</returns>
    let SpreadOptionPricerXGabillon
        inst1
        lags1
        avg1
        inst2
        lags2
        avg2
        slope
        freight
        callput
        expDate
        refMonth
        (pricingDate: DateTime)
        xParam
        pricecurve1
        volcurve1
        pricecurve2
        volcurve2
        o
        =
        SpreadOptionPricerXGabillonWeighted
            inst1
            lags1
            avg1
            inst2
            lags2
            avg2
            slope
            freight
            callput
            expDate
            refMonth
            (pricingDate: DateTime)
            xParam
            pricecurve1
            volcurve1
            pricecurve2
            volcurve2
            o
            [| (1.0, 0) |]

    /// <summary>
    /// Prices an Asian option or swaption using the Gabillon model.
    /// Calculates option price, delta, gamma, vega, and theta using numerical differentiation for Greeks.
    /// </summary>
    /// <param name="inst">Instrument name/type.</param>
    /// <param name="lags">Lags array for the instrument.</param>
    /// <param name="avg">Averaging rule for the instrument.</param>
    /// <param name="k">Strike price.</param>
    /// <param name="callput">Option type (Call/Put).</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month.</param>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="gParam">Gabillon model parameters.</param>
    /// <param name="pricecurve">Price curve for the underlying.</param>
    /// <param name="volcurve">Volatility curve for Gabillon covariance calculation.</param>
    /// <returns>An array of (string, float) tuples with pricing results (Option, Delta1, Gamma1, Vega1, Theta1, P1, Intrinsic, Extrinsic, vol1).</returns>
    ///asian and swaption pricer using Gabillon model
    let AsianOptionPricerGabillon
        inst
        lags
        avg
        k
        callput
        expDate
        refMonth
        (pricingDate: DateTime)
        gParam
        pricecurve
        volcurve
        =
        let (f1, fw1, x1, a1) =
            getInputsG pricingDate expDate refMonth lags avg inst 1.0M pricecurve

        let k' = k - a1 // adapte K for past fixings
        let sigma = getGabillonCov inst volcurve gParam x1 pricingDate
        let t1 = x1 |> Array.map (fst >> getTTM pricingDate) |> toVector
        let opt, delta = asianoptionChoi f1 fw1 k' sigma callput
        let h = NumericalDerivative()

        let g =
            (fun (d: float[]) ->
                let f1'' = f1 + d.[0]
                let pd = pricingDate.AddDays d.[2]
                let v' = volcurve.shift (decimal d.[1])
                let s = getGabillonCov inst v' gParam x1 pd
                let opt, _ = asianoptionChoi f1'' fw1 k' s callput
                opt)

        let d = [| 0.0; 0.0; 0.0 |]
        let gamma = h.EvaluatePartialDerivative(g, d, 0, 2) //gamma
        let vega = h.EvaluatePartialDerivative(g, d, 1, 1) / 100. //vega 1 vol
        let theta = h.EvaluatePartialDerivative(g, d, 2, 1) //1d theta
        let p1 = (f1 .* fw1).Sum() + a1 //inst1 forwd

        let pintr =
            match callput with
            | Call -> (max (p1 - k) 0.)
            | Put -> (max (k - p1) 0.)

        let v1 = (sigma.Diagonal() ./ t1).PointwiseSqrt().Mean()

        [| "Option", opt
           "Delta1", delta
           "Gamma1", gamma
           "Vega1", vega
           "Theta1", theta
           "P1", p1
           "Intrinsic", pintr
           "Extrinsic", opt - pintr
           "vol1", v1 |]

    /// <summary>
    /// Prices an Asian option or swaption using a moment-matching approximation.
    /// </summary>
    /// <param name="inst">Instrument name/type.</param>
    /// <param name="lags">Lags array.</param>
    /// <param name="avg">Averaging rule.</param>
    /// <param name="k">Strike price.</param>
    /// <param name="callput">Option type (Call/Put).</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month.</param>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="pricecurve">Price curve.</param>
    /// <param name="volcurve">Volatility curve.</param>
    /// <returns>An array of (string, float) tuples with pricing results (Option, Delta1, P1, Intrinsic, Extrinsic, vol1).</returns>
    ///asian and swaption pricer using moment matching model
    let AsianOptionPricer inst lags avg k callput expDate refMonth (pricingDate: DateTime) pricecurve volcurve =
        let (f1, fw1, t1, v1, a1) =
            getInputs pricingDate expDate refMonth lags avg inst 1.0M pricecurve volcurve

        let opt, delta = asianOptionAndDelta f1 fw1 t1 v1 k callput a1
        let p1 = (f1 .* fw1).Sum() + a1 //inst1 forwd

        let pintr =
            match callput with
            | Call -> (max (p1 - k) 0.)
            | Put -> (max (k - p1) 0.)

        [| "Option", opt
           "Delta1", delta
           "P1", p1
           "Intrinsic", pintr
           "Extrinsic", opt - pintr
           "vol1", v1.Mean() |]

    /// <summary>
    /// Prices an Asian option using a moment-matching model, specifically tailored for scenarios with no past fixings (e.g., ICE settlement style).
    /// Fixings are considered only from the day after the pricingDate.
    /// </summary>
    /// <param name="inst">Instrument name/type.</param>
    /// <param name="lags">Lags array.</param>
    /// <param name="avg">Averaging rule.</param>
    /// <param name="k">Strike price.</param>
    /// <param name="callput">Option type (Call/Put).</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month.</param>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="pricecurve">Price curve.</param>
    /// <param name="volcurve">Volatility curve.</param>
    /// <returns>An array containing a single (string, float) tuple: ("Option", option_price).</returns>
    ///asian and swaption pricer using moment matching model
    ///with no past fixings, seems to be what ICE settlement is for.
    ///This is used for implied asian vol from listed apo settlements.
    let AsianOptionPricer' inst lags avg k callput expDate refMonth (pricingDate: DateTime) pricecurve volcurve =
        let getFixings refMonth (com: Commod) lags slope avg expDate =
            let refDate = refMonth |> pillarToDate
            //get reference contract, swap for oil, bullet for gas
            let avgfwd = getAvgFwd com.Instrument
            let contracts' = getNrbyContracts avgfwd

            lags
            |> Array.map (fun i ->
                let refMonth = refDate.AddMonths i
                let contract = refMonth |> formatPillar

                let d1, d2 =
                    match com.Instrument with
                    | JKM
                    | TTF
                    | NG -> //for gas, use the contract month
                        getContractMonth contracts' contract
                    | _ -> refMonth, dateAdjust' "e" refMonth
                //let dates = getFixingDates avgfwd.Frequency com.Calendar d1 d2
                let dates = getFixingDates avg com.Calendar (max d1 (pricingDate.AddDays(1.0))) d2
                //let contracts = List.replicate dates.Length contract
                let contracts = getFixingContracts contracts' dates

                let weights =
                    (getEqualWeights dates)
                    |> Array.map (fun x -> x / (float lags.Length) * (float slope))

                let fixingDates = dates |> Array.map (fun d -> min d expDate)
                fixingDates, weights, contracts)
            |> Array.reduce (fun (d1, w1, c1) (d2, w2, c2) ->
                (Array.append d1 d2), (Array.append w1 w2), (Array.append c1 c2))
            //consolidate future details to group weightes for same fixing dates and same contracts
            |||> Array.zip3
            |> Array.groupBy (fun (x, _, z) -> x, z)
            |> Array.map (fun ((k1, k2), v) -> k1, (v |> Array.sumBy (fun (_, x, _) -> x)), k2)

        let getInputs pricingDate expDate refMonth lags avg inst slope (pricecurve: PriceCurve) (volcurve: VolCurve) =
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

            let (pastDetails1, futureDetails1) =
                splitDetails pricingDate (getFixings refMonth com lags slope avg expDate)

            let (f1, fw1, d1, v1) = getFutureInputs futureDetails1 getPrices1 getVol
            //let p1 = getPastInputs pastDetails1 (fun _ c -> getPrices1 c )
            let p1 =
                getPastInputs pastDetails1 (fun d c ->
                    match (getfixing inst d) with
                    | Some v -> v
                    | None -> getPrices1 c)

            let t1 = d1 |> Array.map (getTTM pricingDate)
            (toVector f1, toVector fw1, toVector t1, toVector v1, p1)

        let (f1, fw1, t1, v1, a1) =
            getInputs pricingDate expDate refMonth lags avg inst 1.0M pricecurve volcurve

        let opt = asianoption f1 fw1 t1 v1 k callput 0.
        [| "Option", opt |]

    /// <summary>
    /// Prices an Asian option incorporating a volatility smile.
    /// It uses Gabillon-style inputs (`getInputsG`) and numerical differentiation for Greeks.
    /// The volatility is determined by `getRefDelta` based on the smile parameters.
    /// </summary>
    /// <param name="inst">Instrument name/type.</param>
    /// <param name="lags">Lags array.</param>
    /// <param name="avg">Averaging rule.</param>
    /// <param name="k">Strike price.</param>
    /// <param name="callput">Option type (Call/Put).</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month.</param>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="pricecurve">Price curve.</param>
    /// <param name="smile">Volatility smile (VolDeltaSmile object) used to derive volatilities.</param>
    /// <returns>An array of (string, float) tuples with pricing results (Option, Delta1, Gamma1, Vega1, Theta1, P1, Intrinsic, Extrinsic, vol1).</returns>
    let AsianOptionPricerSmile
        inst
        lags
        avg
        k
        callput
        expDate
        refMonth
        (pricingDate: DateTime)
        pricecurve
        (smile: VolDeltaSmile)
        =
        let (f1, fw1, x1, a1) =
            getInputsG pricingDate expDate refMonth lags avg inst 1.0M pricecurve

        let m = x1.Length / 2 //choose middle fixing
        let t', c' = x1.[m]
        let f = f1 * fw1
        let k' = k - a1
        let volcurve = getRefDelta f k' t' c' smile pricingDate

        let getVol c =
            if volcurve.Pillars.Contains c then
                volcurve.Item c
            else
                failwithf "try to get vol:%s from %A" c volcurve

        let v1 = x1 |> Array.map (snd >> getVol) |> toVector
        let h = NumericalDerivative()

        let g =
            (fun (d: float[]) ->
                let f1'' = f1 + d.[0]
                let smile' = smile.Shift d.[1]
                let pd = pricingDate.AddDays d.[2]
                let v' = getRefDelta f k' t' c' smile' pd
                let getVol' c = v'.Item c
                let v'' = x1 |> Array.map (snd >> getVol') |> toVector
                let t'' = x1 |> Array.map (fst >> getTTM pd) |> toVector
                asianoption f1'' fw1 t'' v'' k callput a1)

        let d = [| 0.0; 0.0; 0.0 |]
        let opt = h.EvaluatePartialDerivative(g, d, 0, 0) //opt
        let delta = h.EvaluatePartialDerivative(g, d, 0, 1) //delta
        let gamma = h.EvaluatePartialDerivative(g, d, 0, 2) //gamma
        let vega = h.EvaluatePartialDerivative(g, d, 1, 1) / 100. //vega 1 vol
        let theta = - h.EvaluatePartialDerivative(g, d, 2, 1) / 365. //1d theta
        let p1 = (f1 .* fw1).Sum() + a1 //inst1 forwd

        let pintr =
            match callput with
            | Call -> (max (p1 - k) 0.)
            | Put -> (max (k - p1) 0.)

        [| "Option", opt
           "Delta1", delta
           "Gamma1", gamma
           "Vega1", vega
           "Theta1", theta
           "P1", p1
           "Intrinsic", pintr
           "Extrinsic", opt - pintr
           "vol1", v1.Mean() |]

    /// <summary>
    /// Prices an Asian option using the Gabillon model and incorporating a volatility smile.
    /// Volatility is derived using `getRefDeltaGabillon` which is smile-aware. Greeks are calculated via numerical differentiation.
    /// </summary>
    /// <param name="inst">Instrument name/type.</param>
    /// <param name="lags">Lags array.</param>
    /// <param name="avg">Averaging rule.</param>
    /// <param name="k">Strike price.</param>
    /// <param name="callput">Option type (Call/Put).</param>
    /// <param name="expDate">Expiration date.</param>
    /// <param name="refMonth">Reference month.</param>
    /// <param name="pricingDate">Pricing date.</param>
    /// <param name="gParam">Gabillon model parameters.</param>
    /// <param name="pricecurve">Price curve.</param>
    /// <param name="smile">Volatility smile (VolDeltaSmile object).</param>
    /// <returns>An array of (string, float) tuples with pricing results (Option, Delta1, Gamma1, Vega1, Theta1, P1, Intrinsic, Extrinsic, vol1).</returns>
    ///asian and swaption pricer using Gabillon model
    let AsianOptionPricerSmileGabillon
        inst
        lags
        avg
        k
        callput
        expDate
        refMonth
        (pricingDate: DateTime)
        gParam
        pricecurve
        smile
        =
        let com = getCommod inst

        let (f1, fw1, x1, a1) =
            getInputsG pricingDate expDate refMonth lags avg inst 1.0M pricecurve

        let m = x1.Length / 2 //choose middle fixing
        let t', c' = x1.[m]
        let fe = com.Contracts.Fut.[c']
        let oe = com.Contracts.Opt.[c']
        let f = f1 * fw1
        let k' = k - a1 // adapte K for past fixings
        let volcurve = getRefDeltaGabillon f k' t' c' smile oe fe gParam pricingDate
        let t1 = x1 |> Array.map (fst >> getTTM pricingDate) |> toVector
        let sigma = getGabillonCov inst volcurve gParam x1 pricingDate
        let v1 = (sigma.Diagonal() ./ t1).PointwiseSqrt().Mean()
        let h = NumericalDerivative()

        let g =
            (fun (d: float[]) ->
                let f1'' = f1 + d.[0]
                let f'' = f1 * fw1
                let s'' = smile.Shift d.[1]
                let pd = pricingDate.AddDays d.[2]
                let volcurve = getRefDeltaGabillon f'' k' t' c' s'' oe fe gParam pd
                let sigma' = getGabillonCov inst volcurve gParam x1 pd
                let opt, _ = asianoptionChoi f1'' fw1 k' sigma' callput
                opt)

        let d = [| 0.0; 0.0; 0.0 |]
        let opt = h.EvaluatePartialDerivative(g, d, 0, 0) //opt
        let delta = h.EvaluatePartialDerivative(g, d, 0, 1) //delta
        let gamma = h.EvaluatePartialDerivative(g, d, 0, 2) //gamma
        let vega = h.EvaluatePartialDerivative(g, d, 1, 1) / 100. //vega 1 vol
        let theta = h.EvaluatePartialDerivative(g, d, 2, 1) //1d theta
        let p1 = f + a1 //inst1 forwd

        let pintr =
            match callput with
            | Call -> (max (p1 - k) 0.)
            | Put -> (max (k - p1) 0.)

        [| "Option", opt
           "Delta1", delta
           "Gamma1", gamma
           "Vega1", vega
           "Theta1", theta
           "P1", p1
           "Intrinsic", pintr
           "Extrinsic", opt - pintr
           "vol1", v1 |]
