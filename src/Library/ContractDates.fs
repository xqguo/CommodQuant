﻿namespace Commod
[<AutoOpen>]
//define curve pillars following either published source or date rules.
module ContractDates = 
    open System
    open Utils
    open Deedle
    open Deedle.Internal
    open Calendars

    let brtDates = Frame.ReadCsv<string>(ROOT +/ "holidays" +/ "BrentPillars.csv", indexCol = "Month", inferTypes = false)
    let brtContracts = 
        brtDates.Columns?("Last Trade").As<string>()
        |> Series.filter( fun s v -> s <> "" && v <> "" )
        |> Series.mapValues parseMMddyy 
        |> ContractDates

    //GO contracts: https://www.theice.com/products/34361119/Low-Sulphur-Gasoil-Future
    //compute Last Trading Day: Trading shall cease at 12:00 hours, 2 business days prior to the 14th calendar day of the delivery.
    let goContracts = 
        ///start from current month last year
        let td = DateTime.Today |> dateAdjust' "-1ya" 
        let goHol = getCalendar GO calendars
        generateMonth (td |> dateAdjust' "a" ) true 
        |> Seq.map ( fun x -> ( x.ToString("MMM-yy"), x |> dateAdjust goHol "13d-2b" ))
        |> Seq.skipWhile( fun (_,d) -> d < td )
        |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
        |> Series.ofObservations
        |> ContractDates

    ///jkm contracts expiration is 15th of m-1, or previous bd. 
    let jkmContracts = 
        ///start from current month last year
        let td = DateTime.Today |> dateAdjust' "-1ya" 
        let jkmHol = getCalendar JKM calendars
        generateMonth (td |> dateAdjust' "a" ) true 
        |> Seq.map ( fun x -> ( x.ToString("MMM-yy"), x |> dateAdjust jkmHol "-1m15d-1b" ))
        |> Seq.skipWhile( fun (_,d) -> d < td )
        |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
        |> Series.ofObservations
        |> ContractDates

    let getJkmPeriod x = 
        let (d0, d1 ) = getPeriod x 
        let jkmHol = getCalendar JKM calendars
        let d0' = dateAdjust jkmHol "-2ma15d-1b+1d" d0
        let d1' = dateAdjust jkmHol "-1ma15d-1b" d1
        (d0', d1')

    let jccContracts = 
        ///start from current month last year
        let td = DateTime.Today |> dateAdjust' "-1ya" 
        generateMonth (td |> dateAdjust' "a" ) true 
        |> Seq.map ( fun x -> ( x.ToString("MMM-yy"), x ))
        |> Seq.filter( fun (_,d) -> d >= td )
        |> Seq.take 24
        |> Series.ofObservations
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
        let getExp d = dateAdjust (getCalendar TTF calendars ) "-2b" d
        generateMonth (td |> dateAdjust' "a" ) true 
        |> Seq.map ( fun x -> ( x.ToString("MMM-yy"), (getExp x)  ))
        |> Seq.skipWhile( fun (_,d) -> d < td )
        |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
        |> Series.ofObservations
        |> ContractDates

    let getContracts ins =
        match ins with
        | BRT -> brtContracts
        | JKM -> jkmContracts
        | JCC -> jccContracts
        | TTF -> ttfContracts
        | DBRT | DUB | FO180 | FO380 | FO3_5 | GO | NG | SGO -> invalidOp "not implemented"

