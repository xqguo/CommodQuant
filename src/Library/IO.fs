namespace Commod

[<AutoOpen>]
module IOcsv =
    open System
    open System.IO
    open Deedle
    let mutable ROOT =
        let dllDir = AppDomain.CurrentDomain.BaseDirectory
        let parDir = dllDir +/ ".."
        if Directory.Exists(parDir +/ "csv") then parDir else dllDir

    let hols =
        new System.Collections.Concurrent.ConcurrentDictionary<HolidayCode, Set<DateTime>>()

    let getCalendarbyCode (code: HolidayCode) =
        hols.GetOrAdd(
            code,
            (let f = ROOT +/ "holidays" +/ code.ToString() + ".txt" |> tryFile

             match f with
             | Some path -> path |> readLines |> Seq.choose (parseDateExact "yyyyMMMdd") |> set
             | None -> Set.empty)
        )

    //trading calendar to get fixing dates
    let getCalendar ins =
        match ins with
        | DBRT
        | FO35 -> [ PLTLDN ]
        | JKM
        | SGO
        | SJET
        | FO380
        | FO180
        | MFO
        | DUB -> [ PLTSGP ]
        | BRT
        | GO
        | TTF
        | SPP -> [ ICE ]
        | NBP -> [ ICE; UK ]
        | NG -> [ CME ]
        | JCC
        | _ -> [ ALLDAYS ]
        |> List.fold (fun acc s -> Set.union acc (getCalendarbyCode s)) Set.empty

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

    let mutable USDOISSOURCE = (ROOT +/ "csv" +/ "USD OIS_Rate.csv")

    let mutable fixings =
        tryFile (ROOT +/ "csv" +/ "fixings.csv")
        |> Option.map (fun f -> Frame.ReadCsv(f) |> Frame.indexRowsDate "Date")

    let getfixing (ins: Instrument) (d: DateTime) =
        fixings
        |> Option.bind (fun f -> f.GetColumn(ins.ToString()) |> Series.tryGet d |> Option.map decimal)

    let getfixings (ins: Instrument) (d: DateTime[]) =
        match fixings with
        | Some f ->
            match f.TryGetColumn<decimal>(ins.ToString(), Lookup.Exact) with
            | OptionalValue.Present v ->
                let s = v.[d].DropMissing()

                if s.KeyCount = d.Length then
                    s.Values |> Seq.toArray //|> Array.map decimal
                else
                    let d1 = d |> set
                    let d2 = s.Keys |> set
                    let m = d1 - d2
                    failwith $"missing historical fixing for {ins} on %A{m}"
            | OptionalValue.Missing -> failwith $"missing {ins} in fixing file"
        | None -> failwith $"missing historical fixing file"
