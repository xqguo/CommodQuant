namespace Commod
module IOcsv =
    open System
    open System.IO
    open Utils

    let ROOT = Path.GetDirectoryName( Reflection.Assembly.GetExecutingAssembly().Location)

    let readCalendar f = 
        File.ReadAllLines( ROOT +/ "holidays" +/ f ) 
        |> Array.choose( parseDateExact "yyyyMMMdd" )
        |> set

    let private pltsgpCalendar = readCalendar "pltsgp.txt"
    let private pltldnCalendar = readCalendar "pltldn.txt"
    let private iceCalendar = readCalendar "ice.txt"
    let private nymCalendar = readCalendar "nym.txt"

    let calendars = 
        [ 
            (ICE, iceCalendar)
            (CME, nymCalendar )
            (PLTSGP, pltsgpCalendar)
            (PLTLDN, pltldnCalendar)
        ] 
        |> Map.ofList

    let getCalendar ins (calendars:Map<_,_>) = 
        match ins with
        | DBRT | FO3_5 -> [PLTLDN] |> set
        | JKM | SGO | SJET | FO380 | FO180 | DUB -> [PLTSGP] |> set
        | BRT | GO | TTF  -> [ICE] |> set 
        | JCC -> Set.empty
        | NG -> [CME] |> set
        |> Set.fold ( fun acc s -> Set.union acc calendars.[s] ) Set.empty


    let datamap = 
        [ 
            //ins, price data, futexp, optexp
            BRT , ( "BR ICE_Price.csv", "brtfut.csv", "brtopt.csv" )
        ]
        |> dict

    let tryPriceFile i = 
        if datamap.ContainsKey i then 
            let f,_,_ = datamap.[i] 
            Some (ROOT +/ "csv" +/ f)
        else
            None

    let tryFutExpFile i = 
        if datamap.ContainsKey i then 
            let _,f,_ = datamap.[i]
            Some ( ROOT +/ "holidays" +/ f)
        else
            None

    let USDOISSOURCE = ( ROOT +/ "csv" +/ "USD OIS_Rate.csv" )
