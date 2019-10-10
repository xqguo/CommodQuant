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
        if Directory.Exists( dlldir +/ "csv" ) then 
            dlldir 
        elif 
            Directory.Exists( dir1 +/ "csv" ) then 
            dir1 
        else //use env variable if set 
            let root = Environment.GetEnvironmentVariable("COMMODITIES")
            let dir = 
                match root with
                | null ->  //use one drive for default env variables
                    let dir = Environment.GetEnvironmentVariable("USERPROFILE") 
                                + @"\OneDrive - Pavilion Energy\Commodities\bin"
                                +/ "OneDrive - Pavilion Energy" 
                                +/ "Commodities" 
                                +/ "bin"
                    Environment.SetEnvironmentVariable("COMMODITIES", dir, EnvironmentVariableTarget.User)
                    dir
                | x -> x
            if Directory.Exists( dir +/ "csv" ) then 
                dir 
            else 
                failwith <| sprintf "Invalid root directory for CommodLib: %s" dir

    let addPath () =
        let entries = 
            Environment.GetEnvironmentVariable("Path").Split(';')
            |> Array.filter( fun x -> x <> ROOT)
            |> Array.append( [|ROOT|] )
        let path = String.Join(";", entries)
        Environment.SetEnvironmentVariable("Path", path, EnvironmentVariableTarget.User)

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
