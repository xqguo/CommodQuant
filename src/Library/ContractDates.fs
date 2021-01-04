namespace Commod
[<AutoOpen>]
//define curve pillars following either published source or date rules.
module Contracts = 
    open System

    module Conventions = 
        //let brtDates = Frame.ReadCsv<string>(ROOT +/ "holidays" +/ "BrentPillars.csv", indexCol = "Month", inferTypes = false)
        //let brtContracts = 
        //    brtDates.Columns?("Last Trade").As<string>()
        //    |> Series.filter( fun s v -> s <> "" && v <> "" )
        //    |> Series.mapValues parseMMddyy 
        //    |> ContractDates


        let readContracts (v:string) = 
                ContractCsv.Load( v ).Rows
                |> Seq.map( fun r -> 
                    let pillar = pillarToDate r.Column1 |> formatPillar
                    pillar, r.Column2 ) 
                |> Map.ofSeq 
               // |> ContractDates
        
        // if date is bd before Christmas or New Year, then the bd before
        // the rule seems to be applied using calendar day
        let prevChrismasNY hol (d:DateTime) = 
            match d.Month with
            | 12 -> 
                let cmas = DateTime(d.Year, d.Month, 25 )
                let nyr = DateTime(d.Year + 1, 1, 1)
                if (d = dateAdjust' "-1b" cmas ) || ( d = dateAdjust' "-1b" nyr ) then
                    dateAdjust hol "-1b" d
                else d
            | _ -> d

        //https://www.theice.com/products/219/Brent-Crude-Futures
        //Expiration Date
        //Trading shall cease at the end of the designated settlement period on the last Business Day of the second month preceding the relevant contract month (e.g. the March contract month will expire on the last Business Day of January).
        //if the day on which trading is due to cease would be either: (i) the Business Day preceding Christmas Day, or (ii) the Business Day preceding New Year’s Day, then trading shall cease on the next preceding Business Day
        let getBrtExp month =  
            //let hol = getCalendar BRT
            let hol = Set.union (getCalendar BRT) (getCalendarbyCode UKB)
            dateAdjust hol "a-1m-1b" month |> prevChrismasNY hol 

       
        //https://www.theice.com/products/27996665/Dutch-TTF-Gas-Futures/
        //Expiration Date
        //Trading will cease at the close of business two UK Business Days prior to the first calendar day of the delivery month, quarter, season, or calendar.
        let getTtfExp month =  
            let hol = Set.union (getCalendar TTF) (getCalendarbyCode UKB) //include ICE
            dateAdjust hol "a-2b" month

        //https://www.cmegroup.com/trading/energy/natural-gas/natural-gas_product_calendar_futures.html
        //Expiration Date
        //Trading will cease at the close of business three Business Days prior to the first calendar day of the delivery month, quarter, season, or calendar.
        let getNgExp month =  
            let hol = getCalendar NG
            dateAdjust hol "a-3b" month
        
        //GO contracts: https://www.theice.com/products/34361119/Low-Sulphur-Gasoil-Future
        //compute Last Trading Day: Trading shall cease at 12:00 hours, 2 business days prior to the 14th calendar day of the delivery.
        let getGoExp month =  
            let hol = getCalendar GO
            dateAdjust hol "a13d-2b" month

        ///jkm contracts expiration is 15th of m-1, or previous bd. 
        let getJkmExp month =  
            let hol = getCalendar JKM
            dateAdjust hol "a-1m15d-1b" month

        //use 1 bd of the month
        let getJccExp month = 
            let hol = getCalendar JCC
            dateAdjust hol "an" month

        let getExp d ins =
            match ins with 
            | BRT -> getBrtExp d 
            | TTF -> getTtfExp d 
            | GO -> getGoExp d
            | JKM -> getJkmExp d
            | JCC -> getJccExp d
            | _ -> dateAdjust ( getCalendar ins ) "ep" d

        //https://www.theice.com/products/218/Brent-Crude-American-style-Options
        //Last Trading Day
        //Trading shall cease at the end of the designated settlement period of the ICE Brent Crude Futures Contract three Business Days before the scheduled cessation of trading for the relevant contract month of the ICE Brent Crude Futures Contract.
        //If the day on which trading in the relevant option is due to cease would be either: (i) the Business Day preceding Christmas Day, or (ii) the Business Day preceding New Year’s Day, then trading shall cease on the immediately preceding Business Day
        let getBrtOptExp month =  
            let hol = Set.union (getCalendar BRT) (getCalendarbyCode UKB)
            getExp month BRT |> dateAdjust hol "-3b" |> prevChrismasNY hol
        //getBrtOptExp (DateTime(2020,2,1))

        //https://www.theice.com/products/71085679/Dutch-TTF-Gas-Options-Futures-Style-Margin
        //Expiration Date
        //Trading will cease when the intraday reference price is set , approximately 14:00 CET (as specified in the Operating Schedule - Appendix B.1), of the underlying futures contract five calendar days before the start of the contract month. If that day is a non-business day, expiry will occur on the nearest prior business day, except where that day is also the expiry date of the underlying futures contract, in which case expiry will occur on the preceding business day.
        let getTtfOptExp month = 
            //let hol = getCalendar TTF
            let hol = Set.union (getCalendar TTF) (getCalendarbyCode UK)
            let fut = getExp month TTF
            let opt = dateAdjust hol "a-5dp" month 
            if fut = opt then 
                dateAdjust hol "-1b" opt
            else opt

        let getOptExp d ins =
            match ins with 
            | BRT -> getBrtOptExp d 
            | TTF -> getTtfOptExp d 
            //| GO -> getGoExp d
            //| JKM -> getJkmExp d
            //| JCC -> getJccExp d
            | _ -> dateAdjust ( getCalendar ins ) "ep" d
            
    let getContracts ins= 
        let f = tryFutExpFile ins
        let actuals = 
            match f with 
            | Some v -> Conventions.readContracts v
            | None -> Map.empty
        let rulebased =
            //generate last bd of prior month
            let td = DateTime.Today |> dateAdjust' "-1ya" 
            generateMonth (td |> dateAdjust' "a" ) true 
            |> Seq.map ( fun x -> (formatPillar x , Conventions.getExp x ins))
            |> Seq.skipWhile( fun (_,d) -> d < td )
            |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
            |> Map.ofSeq
        //use actuals to override rulebased
        let contracts = Map.fold (fun acc key value -> Map.add key value acc) rulebased actuals
        ContractDates contracts

        // let getContractPeriod ins d = 
            
        //     let d0 = getExp ( dateAdjust' "-1ma" d) ins
        //     let d1 = getExp d ins
        //     d0.AddDays(1.), d1

        // let getJkmPeriod month = getContractPeriod JKM month

    let getOptContracts ins= 
        let f = tryOptExpFile ins
        let actuals = 
            match f with 
            | Some v -> Conventions.readContracts v
            | None -> Map.empty
        let rulebased =
            let td = DateTime.Today |> dateAdjust' "-1ya" 
            generateMonth (td |> dateAdjust' "a" ) true 
            |> Seq.map ( fun x -> (formatPillar x , Conventions.getOptExp x ins))
            |> Seq.skipWhile( fun (_,d) -> d < td )
            |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
            |> Map.ofSeq
        //use actuals to override rulebased
        let contracts = Map.fold (fun acc key value -> Map.add key value acc) rulebased actuals
        ContractDates contracts
