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
        
        let getBrtExp month =  
            let hol = getCalendar BRT
            dateAdjust hol "a-1mp" month

        //https://www.theice.com/products/27996665/Dutch-TTF-Gas-Futures/
        //Expiration Date
        //Trading will cease at the close of business two Business Days prior to the first calendar day of the delivery month, quarter, season, or calendar.
        let getTtfExp month =  
            let hol = getCalendar TTF
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
            
    let getContracts ins= 
        let f = tryFutExpFile ins
        let actuals = 
            match f with 
            | Some v -> Conventions.readContracts v
            | None -> Map.empty
        let rulebased =
            //generate last bd of prior month
            let td = DateTime.Today |> dateAdjust' "-1ya" 
            let hol = getCalendar ins
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
