namespace Commod

[<AutoOpen>]
module IOcsv =
    open System
    open System.IO
    open Deedle
    /// Specifies the root directory for CSV files.
    /// It defaults to the 'csv' directory in the parent directory of the DLL,
    /// or the DLL directory if 'csv' doesn't exist.
    let mutable ROOT =
        let dllDir = AppDomain.CurrentDomain.BaseDirectory
        let parDir = dllDir +/ ".."
        if Directory.Exists(parDir +/ "csv") then parDir else dllDir

    /// Cache for holiday calendars.
    let hols =
        new System.Collections.Concurrent.ConcurrentDictionary<HolidayCode, Set<DateTime>>()

    /// <summary>
    /// Gets a holiday calendar by its code.
    /// </summary>
    /// <param name="code">The holiday code.</param>
    /// <returns>A set of DateTime objects representing the holidays.</returns>
    let getCalendarbyCode (code: HolidayCode) =
        hols.GetOrAdd(
            code,
            (let f = ROOT +/ "holidays" +/ code.ToString() + ".txt" |> tryFile

             match f with
             | Some path -> path |> readLines |> Seq.choose (parseDateExact "yyyyMMMdd") |> set
             | None -> Set.empty)
        )

    /// <summary>
    /// Gets the trading calendar for a given instrument.
    /// </summary>
    /// <param name="ins">The instrument.</param>
    /// <returns>A set of DateTime objects representing the trading calendar.</returns>
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

    /// <summary>
    /// Tries to find the price file for a given instrument.
    /// </summary>
    /// <param name="i">The instrument.</param>
    /// <returns>The file path if found, otherwise None.</returns>
    let tryPriceFile i =
        ROOT +/ "csv" +/ (i.ToString() + "_Price.csv") |> tryFile

    /// <summary>
    /// Tries to find the volatility file for a given instrument.
    /// </summary>
    /// <param name="i">The instrument.</param>
    /// <returns>The file path if found, otherwise None.</returns>
    let tryVolsFile i =
        ROOT +/ "csv" +/ (i.ToString() + "_Vol.csv") |> tryFile

    /// <summary>
    /// Tries to find the smile file for a given instrument.
    /// </summary>
    /// <param name="i">The instrument.</param>
    /// <returns>The file path if found, otherwise None.</returns>
    let trySmileFile i =
        ROOT +/ "csv" +/ (i.ToString() + "_Smile.csv") |> tryFile

    /// <summary>
    /// Tries to find the future expiration file for a given instrument.
    /// </summary>
    /// <param name="i">The instrument.</param>
    /// <returns>The file path if found, otherwise None.</returns>
    let tryFutExpFile i =
        ROOT +/ "holidays" +/ (i.ToString() + "fut.csv") |> tryFile

    /// <summary>
    /// Tries to find the option expiration file for a given instrument.
    /// </summary>
    /// <param name="i">The instrument.</param>
    /// <returns>The file path if found, otherwise None.</returns>
    let tryOptExpFile i =
        ROOT +/ "holidays" +/ (i.ToString() + "opt.csv") |> tryFile

    /// Specifies the source file for USD OIS rates.
    let mutable USDOISSOURCE = (ROOT +/ "csv" +/ "USD OIS_Rate.csv")

    /// Stores the fixings data, loaded from "fixings.csv".
    let mutable fixings =
        tryFile (ROOT +/ "csv" +/ "fixings.csv")
        |> Option.map (fun f ->
            Frame.ReadCsv(f) |> Frame.indexRowsDate "Date")

    /// <summary>
    /// Adds fixings for a given instrument.
    /// </summary>
    /// <param name="ins">The instrument.</param>
    /// <param name="d">Array of dates.</param>
    /// <param name="v">Array of values.</param>
    let addfixings ins d v  =
        let k = ins.ToString()
        let s0 = Array.zip d v |> series
        match fixings with
        | Some f ->
            let s = f.TryGetColumn<float>(k, Lookup.Exact)
            match s with
            | OptionalValue.Present ss ->
                let s1 = Series.mergeUsing UnionBehavior.PreferLeft s0 ss
                f.[k] <- s1
                fixings <- Some f
            | OptionalValue.Missing ->
                f.[k] <- s0
                fixings <- Some f
        | None ->
            fixings <- [k => s0] |> frame |> Some

    /// <summary>
    /// Gets the fixing for a given instrument on a specific date.
    /// </summary>
    /// <param name="ins">The instrument.</param>
    /// <param name="d">The date.</param>
    /// <returns>The fixing value if found, otherwise None.</returns>
    let getfixing (ins: Instrument) (d: DateTime) =
        fixings
        |> Option.bind (fun f ->
            f.GetColumn(ins.ToString()) |> Series.tryGet d |> Option.map decimal)

    /// <summary>
    /// Gets the fixings for a given instrument on specific dates.
    /// </summary>
    /// <param name="ins">The instrument.</param>
    /// <param name="d">Array of dates.</param>
    /// <returns>Array of fixing values.</returns>
    /// <exception cref="FailFastException">Thrown when historical fixing is missing or instrument/file is not found.</exception>
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
