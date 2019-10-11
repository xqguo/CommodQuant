namespace Commod
[<AutoOpen>]
module IOcsv =
    open System
    open System.IO
    open Utils

    ///root dir to read cvs files. 
    ///default to dll dir, then environemnt variable COMMODITIES
    let ROOT = 
        let dlldir = Path.GetDirectoryName( Reflection.Assembly.GetAssembly(typeof<QuantityAmount>).Location)
        let dir1 = @"C:\Commodities\bin" 
         //use env variable if set 
        let root = Environment.GetEnvironmentVariable("COMMODITIES")
        if Directory.Exists( root +/ "csv" ) then 
            root 
        elif Directory.Exists( dlldir +/ "csv" ) then 
            dlldir 
        elif 
            Directory.Exists( dir1 +/ "csv" ) then 
            dir1 
        else //use env variable if set 
            failwith "Cannot find root directory for CommodLib"

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
        | DBRT | FO35 -> [PLTLDN] 
        | JKM | SGO | SJET | FO380 | FO180 | DUB -> [PLTSGP] 
        | BRT | GO | TTF  -> [ICE] 
        | NG -> [CME] 
        | JCC -> [ ALLDAYS ] 
        |> List.fold ( fun acc s -> Set.union acc ( getCalendarbyCode s ) ) Set.empty


    let tryPriceFile i = 
        ROOT +/ "csv" +/ (i.ToString() + "_Price.csv") |> tryFile

    let tryVolsFile i = 
        ROOT +/ "csv" +/ (i.ToString() + "_Vol.csv") |> tryFile

    let tryFutExpFile i = 
        ROOT +/ "holidays" +/ (i.ToString() + "fut.csv") |> tryFile

    let tryOptExpFile i = 
        ROOT +/ "holidays" +/ (i.ToString() + "fut.csv") |> tryFile

    let USDOISSOURCE = ( ROOT +/ "csv" +/ "USD OIS_Rate.csv" )
