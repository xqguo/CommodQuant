namespace Commod
[<AutoOpen>]
module IOcsv =
    open System
    open System.IO
    open Deedle
    //open Serilog

    let mutable ROOT = 
        let dllDir = 
            Path.GetDirectoryName( Reflection.Assembly.GetAssembly(typeof<QuantityAmount>).Location) 
        let parDir = dllDir +/ ".."
        if Directory.Exists (parDir +/ "csv") then parDir else dllDir

    //test logging
    //let logfile = ROOT +/ "logs" +/ "commod.log" 
    //let logLevel = Events.LogEventLevel.Information
    //let mutable logger = 
    //    LoggerConfiguration()
    //        .MinimumLevel.Verbose()
    //        .MinimumLevel.Information() //log info as default, change to verbose for more logging.
    //        //.WriteTo.File(logfile,logLevel)
    //        .WriteTo.File(logfile,logLevel)
    //        .CreateLogger()
    
    //logger.Information <| sprintf "Loading CommodLib" 
    //logger.Debug <| sprintf "Debug CommodLib" 

    ///root dir to read cvs files. 
    ///default to parent of dll bin dir

    let getCalendarbyCode (code:HolidayCode) = 
        let f = ROOT +/ "holidays" +/ code.ToString() + ".txt"  |> tryFile
        match f with 
        | Some path -> 
            path
            |> readLines
            |> Seq.choose( parseDateExact "yyyyMMMdd" )
            |> set
        | None -> Set.empty

    //trading calendar to get fixing dates
    let getCalendar ins = 
        match ins with
        | DBRT | FO35 -> [PLTLDN] 
        | JKM | SGO | SJET | FO380 | FO180 |MFO | DUB -> [PLTSGP] 
        | BRT | GO | TTF -> [ICE] 
        | NBP -> [ICE;UK] 
        | NG -> [CME] 
        | JCC  -> [ ALLDAYS ] 
        |> List.fold ( fun acc s -> Set.union acc ( getCalendarbyCode s ) ) Set.empty


    let tryPriceFile i = 
        ROOT +/ "csv" +/ (i.ToString() + "_Price.csv") |> tryFile

    let tryVolsFile i = 
        ROOT +/ "csv" +/ (i.ToString() + "_Vol.csv") |> tryFile

    let trySmileFile i = 
        ROOT +/ "csv" +/ (i.ToString() + "_Smile.csv") |> tryFile

    let tryFutExpFile i = 
        ROOT +/ "holidays" +/ (i.ToString() + "fut.csv") |> tryFile

    let tryOptExpFile i = 
        ROOT +/ "holidays" +/ (i.ToString() + "opt.csv") |> tryFile

    let USDOISSOURCE = ( ROOT +/ "csv" +/ "USD OIS_Rate.csv" )

    let fixings = Frame.ReadCsv( ROOT +/ "csv" +/ "fixings.csv" ) |> Frame.indexRowsDate "Date"

    let getfixing (ins:Instrument) (d:DateTime) = 
        fixings.GetColumn (ins.ToString()) |> Series.tryGet d |> Option.map decimal

    let getfixings (ins:Instrument) (d:DateTime[]) = 
        let s = (fixings.GetColumn<decimal> (ins.ToString())).[d].DropMissing()        
        if s.KeyCount = d.Length then
            s.Values |> Seq.toArray //|> Array.map decimal
        else
            let d1 = d |> set
            let d2 = s.Keys |> set
            let m = d1 - d2 
            failwith $"missing historical fixing for {ins} on %A{m}" 


        
