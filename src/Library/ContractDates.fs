namespace Commod

[<AutoOpen>]
//define curve pillars following either published source or date rules.
module Contracts =
    open System

    /// <summary>
    /// Provides conventions and rules for determining contract expiration dates for various commodities.
    /// </summary>
    module Conventions =
        //let brtDates = Frame.ReadCsv<string>(ROOT +/ "holidays" +/ "BrentPillars.csv", indexCol = "Month", inferTypes = false)
        //let brtContracts =
        //    brtDates.Columns?("Last Trade").As<string>()
        //    |> Series.filter( fun s v -> s <> "" && v <> "" )
        //    |> Series.mapValues parseMMddyy
        //    |> ContractDates


        /// <summary>
        /// Reads contract dates from a CSV file. The CSV should have two columns:
        /// Column1 for the pillar string (e.g., "Jan24", "Q124") and Column2 for the date string.
        /// </summary>
        /// <param name="v">The path to the CSV file.</param>
        /// <returns>A map of pillar strings (formatted as "MMM-yy") to DateTime objects.</returns>
        let readContracts (v: string) =
            ContractCsv.Load(v).Rows
            |> Seq.map (fun r ->
                let pillar = pillarToDate r.Column1 |> formatPillar
                pillar, r.Column2)
            |> Map.ofSeq
        // |> ContractDates

        /// <summary>
        /// Adjusts a date if it falls on the business day before Christmas or New Year.
        /// If the input date is the business day immediately preceding Christmas Day (Dec 25th)
        /// or New Year’s Day (Jan 1st), the function returns the business day prior to the input date.
        /// Otherwise, the original date is returned.
        /// This is used for Brent futures expiration, where the last trading day is adjusted if 
        /// it falls on the day before Christmas or New Year.
        /// </summary>
        /// <param name="hol">A set of holiday dates to consider for business day calculations.</param>
        /// <param name="d">The date to adjust.</param>
        /// <returns>The adjusted date if applicable; otherwise, the original date.</returns>
        let prevChrismasNY hol (d: DateTime) =
            match d.Month with
            | 12 ->
                let cmas = DateTime(d.Year, d.Month, 25)
                let nyr = DateTime(d.Year + 1, 1, 1)

                if (d = dateAdjust' "-1b" cmas) || (d = dateAdjust' "-1b" nyr) then
                    dateAdjust hol "-1b" d
                else
                    d
            | _ -> d

        /// <summary>
        /// Gets the expiration date for Brent (BRT) futures.
        /// Rule: Trading shall cease at the end of the designated settlement period on the last Business Day
        /// of the second month preceding the relevant contract month (e.g., the March contract month will expire
        /// on the last Business Day of January).
        /// If the day on which trading is due to cease would be either: (i) the Business Day preceding Christmas Day,
        /// or (ii) the Business Day preceding New Year’s Day, then trading shall cease on the next preceding Business Day.
        /// (Source: https://www.theice.com/products/219/Brent-Crude-Futures)
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The expiration date for Brent futures.</returns>
        let getBrtExp month =
            let hol = Set.union (getCalendar BRT) (getCalendarbyCode UK)
            //let hol = getCalendar BRT
            dateAdjust hol "a-1m-1b" month |> prevChrismasNY hol

        /// <summary>
        /// Gets the expiration date for TTF (Dutch Title Transfer Facility) gas futures.
        /// Rule: Trading will cease at the close of business two UK Business Days prior to the
        /// first calendar day of the delivery month, quarter, season, or calendar.
        /// (Source: https://www.theice.com/products/27996665/Dutch-TTF-Gas-Futures/)
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The expiration date for TTF futures.</returns>
        let getTtfExp month =
            let hol = Set.union (getCalendar TTF) (getCalendarbyCode UK) //include ICE
            //let hol = getCalendar TTF
            dateAdjust hol "a-2b" month

        /// <summary>
        /// Gets the expiration date for Natural Gas (NG) futures (Henry Hub).
        /// Rule: Trading will cease at the close of business three Business Days prior to the
        /// first calendar day of the delivery month.
        /// (Source: https://www.cmegroup.com/trading/energy/natural-gas/natural-gas_product_calendar_futures.html)
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The expiration date for NG futures.</returns>
        let getNgExp month =
            let hol = getCalendar NG
            dateAdjust hol "a-3b" month

        /// <summary>
        /// Gets the expiration date for NBP (UK National Balancing Point) gas futures.
        /// Rule: Trading will cease at the close of business two Business Days prior to the
        /// first calendar day of the delivery month.
        /// (Source: https://www.theice.com/products/910/UK-Natural-Gas-Futures)
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The expiration date for NBP futures.</returns>
        let getNbpExp month =
            let hol = getCalendar NBP
            dateAdjust hol "a-2b" month

        /// <summary>
        /// Gets the expiration date for Gas Oil (GO) futures.
        /// Rule: Trading shall cease at 12:00 hours, 2 business days prior to the 14th calendar day of the delivery month.
        /// (Source: https://www.theice.com/products/34361119/Low-Sulphur-Gasoil-Future)
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The expiration date for Gas Oil futures.</returns>
        let getGoExp month =
            let hol = getCalendar GO
            dateAdjust hol "a13d-2b" month // Adjust to 13th day then 2 business days prior (14th - 2b)

        /// <summary>
        /// Gets the expiration date for JKM (Japan Korea Marker) LNG futures.
        /// Rule: JKM contracts expiration is the last business day of the month prior to the contract month,
        /// unless that day is a non-business day, then it's the preceding business day.
        /// (Simplified here as 15th of m-1, adjusted to previous business day - check official source for precision)
        /// A common rule is "Last business day of the month prior to the contract month".
        /// The "a-1m15d-1b" might be a specific interpretation or simplification.
        /// For precision, refer to: https://www.cmegroup.com/trading/energy/lng/japan-korea-marker-lng-platts-swap-futures_product_calendar_futures.html
        /// This usually means last business day of M-1. The code uses "a-1m15d-1b", which aims for mid-month of M-1.
        /// Let's assume the "a-1m15d-1b" is a specific simplification used in this library.
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The expiration date for JKM futures based on the implemented rule.</returns>
        let getJkmExp month =
            let hol = getCalendar JKM
            dateAdjust hol "a-1m15d-1b" month

        /// <summary>
        /// Gets the expiration date for JCC (Japan Crude Cocktail) futures.
        /// JCC is effectively a basket of oil prices with lags.
        /// The rule used here is the first business day of the contract month.
        /// Note: JCC pricing is complex and often involves specific settlement periods not captured by a single expiry date.
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The expiration date for JCC futures based on the implemented rule (first business day of month).</returns>
        let getJccExp month =
            let hol = getCalendar JCC
            dateAdjust hol "an" month

        /// <summary>
        /// Gets the futures expiration date for a given instrument and contract month.
        /// </summary>
        /// <param name="d">The contract month (first day of the month).</param>
        /// <param name="ins">The instrument type. See <see cref="Instruments"/>.</param>
        /// <returns>The calculated expiration date.</returns>
        let getExp d ins =
            match ins with
            | BRT -> getBrtExp d
            | TTF -> getTtfExp d
            | NG -> getNgExp d
            | NBP -> getNbpExp d
            | GO -> getGoExp d
            | JKM -> getJkmExp d
            | JCC -> getJccExp d
            | _ -> dateAdjust (getCalendar ins) "ep" d // Default: end of prior month

        /// <summary>
        /// Gets the option expiration date for Brent (BRT).
        /// Rule: Trading shall cease at the end of the designated settlement period of the
        /// ICE Brent Crude Futures Contract three Business Days before the scheduled 
        /// cessation of trading for the relevant contract month of the ICE Brent Crude Futures Contract.
        /// If the day on which trading in the relevant option is due to cease would be 
        /// either: (i) the Business Day preceding Christmas Day, or (ii) the Business Day preceding New Year’s Day,
        /// then trading shall cease on the immediately preceding Business Day.
        /// (Source: https://www.theice.com/products/218/Brent-Crude-American-style-Options)
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The option expiration date for Brent.</returns>
        let getBrtOptExp month =
            let hol = Set.union (getCalendar BRT) (getCalendarbyCode UK)
            getExp month BRT |> dateAdjust hol "-3b" |> prevChrismasNY hol
        //getBrtOptExp (DateTime(2020,2,1))

        /// <summary>
        /// Gets the option expiration date for TTF (Dutch Title Transfer Facility) gas.
        /// Rule: Trading will cease when the intraday reference price is set (approx. 14:00 CET)
        /// of the underlying futures contract five calendar days before the start of the contract month. 
        /// If that day is a non-business day, expiry will occur on the nearest prior business day, 
        /// except where that day is also the expiry date of the underlying futures contract, 
        /// in which case expiry will occur on the preceding business day.
        /// (Source: https://www.theice.com/products/71085679/Dutch-TTF-Gas-Options-Futures-Style-Margin)
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The option expiration date for TTF gas.</returns>
        let getTtfOptExp month =
            //let hol = getCalendar TTF
            let hol = Set.union (getCalendar TTF) (getCalendarbyCode UK)
            let fut = getExp month TTF
            let opt = dateAdjust hol "a-5dp" month // 5 calendar days prior, adjusted to previous business day
            if fut = opt then dateAdjust hol "-1b" opt else opt

        /// <summary>
        /// Gets the option expiration date for NBP (UK National Balancing Point) gas.
        /// Rule: Trading will cease when the intraday reference price is set (12:50 - 13:00 LLT)
        /// of the underlying futures contract five calendar days before the start of the contract month. 
        /// If that day is a non-business day, expiry will occur on the nearest prior business day, 
        /// except where that day is also the expiry date of the underlying futures contract, 
        /// in which case expiry will occur on the preceding business day.
        /// (Source: https://www.theice.com/products/71085728/UK-Natural-Gas-Options-Futures-Style-Margin)
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The option expiration date for NBP gas.</returns>
        let getNbpOptExp month =
            //let hol = getCalendar TTF
            let hol = Set.union (getCalendar NBP) (getCalendarbyCode UK)
            let fut = getExp month NBP
            let opt = dateAdjust hol "a-5dp" month // 5 calendar days prior, adjusted to previous business day
            if fut = opt then dateAdjust hol "-1b" opt else opt

        /// <summary>
        /// Gets the option expiration date for Natural Gas (NG) futures (Henry Hub).
        /// Rule: Typically expires one business day before the underlying future's expiration.
        /// </summary>
        /// <param name="month">The contract month (first day of the month).</param>
        /// <returns>The option expiration date for NG futures.</returns>
        let getNgOptExp month =
            let hol = getCalendar NG
            let fut = getExp month NG
            dateAdjust hol "-1b" fut

        /// <summary>
        /// Gets the option expiration date for a given instrument and contract month.
        /// For instruments not explicitly listed (GO, JKM, JCC, etc.), it defaults to their future expiration date.
        /// </summary>
        /// <param name="d">The contract month (first day of the month).</param>
        /// <param name="ins">The instrument type. See <see cref="Instruments"/>.</param>
        /// <returns>The calculated option expiration date.</returns>
        let getOptExp d ins =
            match ins with
            | BRT -> getBrtOptExp d
            | TTF -> getTtfOptExp d
            | NBP -> getNbpOptExp d
            | NG -> getNgOptExp d
            //| GO -> getGoExp d // GO options might have different rules, defaulting to future expiry
            //| JKM -> getJkmExp d // JKM options might have different rules, defaulting to future expiry
            //| JCC -> getJccExp d // JCC options might have different rules, defaulting to future expiry
            | _ -> getExp d ins // Default for others: option expiry is same as future expiry

    //let getFutContracts' ins=
    //    let f = tryFutExpFile ins
    //    let actuals =
    //        match f with
    //        | Some v -> Conventions.readContracts v
    //        | None -> Map.empty
    //    let rulebased =
    //        //generate last bd of prior month
    //        let td = DateTime.Today |> dateAdjust' "-1ya"
    //        generateMonth (td |> dateAdjust' "a" ) true
    //        |> Seq.takeWhile( fun d -> d.Year < 2041 )
    //        |> Seq.map ( fun x -> (formatPillar x , Conventions.getExp x ins))
    //        |> Map.ofSeq
    //    //use actuals to override rulebased
    //    Map.fold (fun (acc:Map<string,DateTime>) key value ->
    //        if (acc.ContainsKey key) then
    //            Map.add key value acc
    //        else acc ) rulebased actuals

    //let getOptContracts' ins=
    //    let f = tryOptExpFile ins
    //    let actuals =
    //        match f with
    //        | Some v -> Conventions.readContracts v
    //        | None -> Map.empty
    //    let rulebased =
    //        let td = DateTime.Today |> dateAdjust' "-1ya"
    //        generateMonth (td |> dateAdjust' "a" ) true
    //        |> Seq.takeWhile( fun d -> d.Year < 2041 )
    //        |> Seq.map ( fun x -> (formatPillar x , Conventions.getOptExp x ins))
    //        |> Map.ofSeq
    //    //use actuals to override rulebased
    //    Map.fold (fun (acc:Map<string,DateTime>) key value ->
    //        if (acc.ContainsKey key) then
    //            Map.add key value acc
    //        else acc ) rulebased actuals

    /// <summary>
    /// Defines the default start date for generating rule-based contracts. Mutable.
    /// </summary>
    let mutable contractStart = DateTime(2020, 1, 1)
    /// <summary>
    /// Defines the default end date (exclusive) for generating rule-based contracts. Mutable.
    /// </summary>
    let mutable contractEnd = DateTime(2051, 1, 1)
    /// <summary>
    /// Gets generic contract dates by combining actuals from a file (if available) and rule-based generation.
    /// Actual dates from the file will override rule-based dates for the same pillar.
    /// </summary>
    /// <param name="getfile">A function that takes an instrument and returns an optional file path string for actual contract dates.</param>
    /// <param name="getrule">A function that takes a contract month (DateTime) and an instrument and returns the rule-based contract date (DateTime).</param>
    /// <param name="ins">The instrument. See <see cref="Instruments"/>.</param>
    /// <returns>A map of pillar strings (e.g., "Jan-24") to DateTime objects representing contract dates.</returns>
    let getGenericContracts getfile getrule ins =
        let f = getfile ins

        let actuals =
            match f with
            | Some v -> Conventions.readContracts v
            | None -> Map.empty

        let rulebased =
            //let td = DateTime.Today |> dateAdjust' "-1ya" // Original line, seems unused for contractStart

            generateMonth (contractStart |> dateAdjust' "a") true // Ensure starting from the beginning of the month
            |> Seq.takeWhile (fun d -> d < contractEnd )
            |> Seq.map (fun x -> (formatPillar x, getrule x ins))
            |> Map.ofSeq
        //use actuals to override rulebased
        Map.fold
            (fun (acc: Map<string, DateTime>) key value -> if (acc.ContainsKey key) then Map.add key value acc else acc) // This logic seems to only add if key exists, effectively actuals override rulebased IF rulebased has the key
            rulebased // Start with rulebased
            actuals // Fold actuals onto rulebased, overriding where keys match

    /// <summary>
    /// Gets future contract dates for a given instrument.
    /// It combines dates read from a specific file (if found via `tryFutExpFile`)
    /// with rule-based dates generated by `Conventions.getExp`.
    /// </summary>
    /// <param name="ins">The instrument. See <see cref="Instruments"/>.</param>
    /// <returns>A map of pillar strings to DateTime objects representing future contract dates.</returns>
    let getFutContracts ins =
        getGenericContracts tryFutExpFile Conventions.getExp ins

    /// <summary>
    /// Gets option contract dates for a given instrument.
    /// It combines dates read from a specific file (if found via `tryOptExpFile`)
    /// with rule-based dates generated by `Conventions.getOptExp`.
    /// </summary>
    /// <param name="ins">The instrument. See <see cref="Instruments"/>.</param>
    /// <returns>A map of pillar strings to DateTime objects representing option contract dates.</returns>
    let getOptContracts ins =
        getGenericContracts tryOptExpFile Conventions.getOptExp ins

    /// <summary>
    /// Gets both future and option contract dates for a given instrument.
    /// </summary>
    /// <param name="ins">The instrument. See <see cref="Instruments"/>.</param>
    /// <returns>A <see cref="ContractDates"/> object containing maps for future and option expiration dates.</returns>
    let getContracts ins =
        let opt = getOptContracts ins
        getFutContracts ins |> Map.map (fun k v -> v, opt.[k]) |> ContractDates
