namespace Commod
[<AutoOpen>]
module IOcsv =
    open System
    open System.IO
    ///root dir to read cvs files. 
    ///default to parent of dll bin dir
    let mutable ROOT = 
        let dllDir = 
            Path.GetDirectoryName( Reflection.Assembly.GetAssembly(typeof<QuantityAmount>).Location) 
        let parDir = dllDir +/ ".."
        if Directory.Exists (parDir +/ "csv") then parDir else dllDir

    let getCalendarbyCode (code:HolidayCode) = 
        let f = ROOT +/ "holidays" +/ code.ToString() + ".txt"  |> tryFile
        match f with 
        | Some path -> 
            path
            |> readLines
            |> Seq.choose( parseDateExact "yyyyMMMdd" )
            |> set
        | None -> Set.empty

    let getCalendar ins = 
        match ins with
        | DBRT | FO35 -> [PLTLDN] 
        | JKM | SGO | SJET | FO380 | FO180 | DUB -> [PLTSGP] 
        | BRT | GO | TTF | NBP -> [ICE] 
        | NG -> [CME] 
        | JCC  -> [ ALLDAYS ] 
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
