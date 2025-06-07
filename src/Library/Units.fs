namespace Commod

[<AutoOpen>]
module Units =
    open System

    /// <summary>
    /// Calculates the total number of hours between two dates, inclusive of the end date.
    /// </summary>
    /// <param name="d1">The start date.</param>
    /// <param name="d2">The end date.</param>
    /// <returns>The total hours as a value with unit <c>&lt;h&gt;</c>.</returns>
    let getHours (d1: DateTime) (d2: DateTime) =
        (d2.AddDays(1.0) - d1).TotalHours * 1.0<h>

    /// <summary>
    /// Calculates the total number of hours between two dates, inclusive of the end date, adjusted for a specific time zone (DST-aware).
    /// </summary>
    /// <param name="d1">The start date.</param>
    /// <param name="d2">The end date.</param>
    /// <param name="tz">The time zone information.</param>
    /// <returns>The total hours as a value with unit <c>&lt;h&gt;</c>, adjusted for DST.</returns>
    let getHoursDST (d1: DateTime) (d2: DateTime) tz =
        //let tz = TimeZoneInfo.FindSystemTimeZoneById("Central European Standard Time")
        let d1' = DateTime.SpecifyKind(d1, DateTimeKind.Unspecified)
        let d2' = DateTime.SpecifyKind(d2.AddDays(1.0), DateTimeKind.Unspecified)
        let d = TimeZoneInfo.ConvertTimeToUtc(d1', tz)
        let d' = TimeZoneInfo.ConvertTimeToUtc(d2', tz)
        (d' - d).TotalHours * 1.0<h>

    /// <summary>
    /// Converts a quantity in MW to MWh, accounting for DST using Central European Standard Time.
    /// </summary>
    /// <param name="d1">The start date.</param>
    /// <param name="d2">The end date.</param>
    /// <param name="q">The quantity in MW.</param>
    /// <returns>The equivalent quantity in MWh.</returns>
    let convertMWtoMWHwDST (d1: DateTime) (d2: DateTime) (q: float<mw>) : float<mwh> =
        let tz = TimeZoneInfo.FindSystemTimeZoneById("Central European Standard Time")
        q * (getHoursDST d1 d2 tz)

    /// <summary>
    /// Converts a quantity in MW to MWh (without DST adjustment).
    /// </summary>
    /// <param name="d1">The start date.</param>
    /// <param name="d2">The end date.</param>
    /// <param name="q">The quantity in MW.</param>
    /// <returns>The equivalent quantity in MWh.</returns>
    let convertMWtoMWH (d1: DateTime) (d2: DateTime) (q: float<mw>) : float<mwh> = q * (getHours d1 d2)

    /// <summary>
    /// Converts a quantity in MWh to MMBTU.
    /// </summary>
    /// <param name="q">The quantity in MWh.</param>
    /// <returns>The equivalent quantity in MMBTU.</returns>
    let convertMWHtoMMBTU (q: float<mwh>) = q / 0.293071<mwh / mmbtu>

    /// <summary>
    /// Converts a quantity in metric tons (MT) to barrels (BBL) for a given instrument.
    /// </summary>
    /// <param name="ins">The instrument (e.g., FO380, FO180, FO, BRT).</param>
    /// <param name="q">The quantity in MT.</param>
    /// <returns>The equivalent quantity in BBL.</returns>
    let convertMTtoBBL ins (q: float<mt>) =
        match ins with
        | FO380
        | FO180
        | FO -> q * 6.35<bbl / mt>
        | BRT -> q * 7.33<bbl / mt>
        | _ -> invalidArg (nameof ins) "{ins}"

    /// <summary>
    /// Converts a quantity in metric tons (MT) to MMBTU.
    /// </summary>
    /// <param name="q">The quantity in MT.</param>
    /// <returns>The equivalent quantity in MMBTU.</returns>
    let convertMTtoMMBTU (q: float<mt>) = q * 49.257899069014<mmbtu / mt>

    /// <summary>
    /// Converts a quantity in cubic meters (m^3) to cubic feet (ft^3).
    /// </summary>
    /// <param name="q">The quantity in m^3.</param>
    /// <returns>The equivalent quantity in ft^3.</returns>
    let convertM3toFT3 (q: float<m^3>) = q * 35.314670<ft^3 / m^3>

    /// <summary>
    /// Converts a quantity in cubic feet (ft^3) to MMBTU.
    /// </summary>
    /// <param name="q">The quantity in ft^3.</param>
    /// <returns>The equivalent quantity in MMBTU.</returns>
    let convertFT3toMMBTU (q: float<ft^3>) = q * 0.001<mmbtu / ft^3>

    /// <summary>
    /// The regasification conversion factor (unitless, value 600.0).
    /// </summary>
    let regas = 600.0
