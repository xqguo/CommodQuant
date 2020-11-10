namespace Commod
[<AutoOpen>]
//define curve pillars following either published source or date rules.
module ContractDates = 
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

        let brtContracts = 
            let ins = BRT
            let f = tryFutExpFile ins
            let actuals = 
                match f with 
                | Some v -> readContracts v
                | None -> Map.empty
            let rulebased =
                //generate last bd of prior month
                let td = DateTime.Today |> dateAdjust' "-1ya" 
                let hol = getCalendar ins
                generateMonth (td |> dateAdjust' "a" ) true 
                |> Seq.map ( fun x -> (formatPillar x , x |> dateAdjust hol "-1mp" ))
                |> Seq.skipWhile( fun (_,d) -> d < td )
                |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
                |> Map.ofSeq
            //use actuals to override rulebased
            let contracts = Map.fold (fun acc key value -> Map.add key value acc) rulebased actuals
            ContractDates contracts

        //GO contracts: https://www.theice.com/products/34361119/Low-Sulphur-Gasoil-Future
        //compute Last Trading Day: Trading shall cease at 12:00 hours, 2 business days prior to the 14th calendar day of the delivery.
        let goContracts = 
            ///start from current month last year
            let td = DateTime.Today |> dateAdjust' "-1ya" 
            let goHol = getCalendar GO 
            generateMonth (td |> dateAdjust' "a" ) true 
            |> Seq.map ( fun x -> ( formatPillar x, x |> dateAdjust goHol "13d-2b" ))
            |> Seq.skipWhile( fun (_,d) -> d < td )
            |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
            |> Map.ofSeq
            |> ContractDates

        ///jkm contracts expiration is 15th of m-1, or previous bd. 
        let jkmContracts = 
            ///start from current month last year
            let td = DateTime.Today |> dateAdjust' "-1ya" 
            let jkmHol = getCalendar JKM 
            generateMonth (td |> dateAdjust' "a" ) true 
            |> Seq.map ( fun x -> ( formatPillar x, x |> dateAdjust jkmHol "-1m15d-1b" ))
            |> Seq.skipWhile( fun (_,d) -> d < td )
            |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
            |> Map.ofSeq
            |> ContractDates

        let getJkmPeriod x = 
            let (d0, d1 ) = getPeriod x 
            let jkmHol = getCalendar JKM
            let d0' = dateAdjust jkmHol "-2ma15d-1b+1d" d0
            let d1' = dateAdjust jkmHol "-1ma15d-1b" d1
            (d0', d1')

        let jccContracts = 
            ///start from current month last year
            let td = DateTime.Today |> dateAdjust' "-1ya" 
            generateMonth (td |> dateAdjust' "a" ) true 
            |> Seq.map ( fun x -> (formatPillar x, x ))
            |> Seq.filter( fun (_,d) -> d >= td )
            |> Seq.take 120
            |> Map.ofSeq
            |> ContractDates

        let genericContracts = 
            ///start from Jan last year, end in 7r dec.
            let td = DateTime.Today |> dateAdjust' "A-1y" 
            let d2 = DateTime.Today |> dateAdjust' "Z+9y" 
            generateMonth td true 
            |> Seq.takeWhile (fun x -> x <= d2)
            |> Seq.map ( fun x -> (formatPillar x, dateAdjust' "e" x  ))
            |> Map.ofSeq
            |> ContractDates

        /// For jcc underlying vol fixing dates, lag by 1 month.
        let getJccVolPeriod x = 
            let (d0, d1 ) = getPeriod x 
            let d0' = dateAdjust' "-1ma" d0
            let d1' = dateAdjust' "-1me" d1
            (d0', d1')    

        let ttfContracts = 
            ///start from current month last year
            let td = DateTime.Today |> dateAdjust' "-1ya" 
            //https://www.theice.com/products/27996665/Dutch-TTF-Gas-Futures/
            //Expiration Date
            //Trading will cease at the close of business two Business Days prior to the first calendar day of the delivery month, quarter, season, or calendar.
            let getExp d = dateAdjust (getCalendar TTF ) "-2b" d
            generateMonth (td |> dateAdjust' "a" ) true 
            |> Seq.map ( fun x -> ( formatPillar x, (getExp x)  ))
            |> Seq.skipWhile( fun (_,d) -> d < td )
            |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
            |> Map.ofSeq
            |> ContractDates

        let ngContracts = 
            ///start from current month last year
            let td = DateTime.Today |> dateAdjust' "-1ya" 
            //https://www.cmegroup.com/trading/energy/natural-gas/natural-gas_product_calendar_futures.html
            //Expiration Date
            //Trading will cease at the close of business three Business Days prior to the first calendar day of the delivery month, quarter, season, or calendar.
            let getExp d = dateAdjust (getCalendar NG ) "-3b" d
            generateMonth (td |> dateAdjust' "a" ) true 
            |> Seq.map ( fun x -> ( formatPillar x, (getExp x)  ))
            |> Seq.skipWhile( fun (_,d) -> d < td )
            |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
            |> Map.ofSeq
            |> ContractDates

    let getContracts ins =
        match ins with
        | BRT -> Conventions.brtContracts
        | JKM -> Conventions.jkmContracts
        | JCC -> Conventions.jccContracts
        | TTF -> Conventions.ttfContracts
        | GO -> Conventions.goContracts
        | NG -> Conventions.ngContracts
        | NBP | DBRT | DUB | FO180 | FO380 | FO35 | MFO | SGO | SJET -> Conventions.genericContracts

