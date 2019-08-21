namespace Commod
module IOcsv =
    open System
    open System.IO
    open Utils

    let ROOT = Path.GetDirectoryName( Reflection.Assembly.GetExecutingAssembly().Location)

    //let readCalendar f = 
    //    File.ReadAllLines( ROOT +/ "holidays" +/ f ) 
    //    |> Array.choose( parseDateExact "yyyyMMMdd" )
    //    |> set

    //let private pltsgpCalendar = readCalendar "pltsgp.txt"
    //let private pltldnCalendar = readCalendar "pltldn.txt"
    //let private iceCalendar = readCalendar "ice.txt"
    //let private nymCalendar = readCalendar "nym.txt"

    //let calendars = 
    //    [ 
    //        (ICE, iceCalendar)
    //        (CME, nymCalendar )
    //        (PLTSGP, pltsgpCalendar)
    //        (PLTLDN, pltldnCalendar)
    //    ] 
    //    |> Map.ofList

    let getCalendarbyCode (code:HolidayCode) = 
        let f = ROOT +/ "holidays" +/ code.ToString() + ".txt"  |> tryFile
        match f with 
        | Some path -> 
            File.ReadAllLines path
            |> Array.choose( parseDateExact "yyyyMMMdd" )
            |> set
        | None -> Set.empty

    let getCalendar ins = 
        match ins with
        | DBRT | FO3_5 -> [PLTLDN] 
        | JKM | SGO | SJET | FO380 | FO180 | DUB -> [PLTSGP] 
        | BRT | GO | TTF  -> [ICE] 
        | NG -> [CME] 
        | JCC -> [ ALLDAYS ] 
        |> List.fold ( fun acc s -> Set.union acc ( getCalendarbyCode s ) ) Set.empty


    let tryPriceFile i = 
        ROOT +/ "csv" +/ (i.ToString() + "_Price.csv") |> tryFile

    let tryFutExpFile i = 
        ROOT +/ "holidays" +/ (i.ToString() + "fut.csv") |> tryFile

    let tryOptExpFile i = 
        ROOT +/ "holidays" +/ (i.ToString() + "fut.csv") |> tryFile

    let USDOISSOURCE = ( ROOT +/ "csv" +/ "USD OIS_Rate.csv" )
