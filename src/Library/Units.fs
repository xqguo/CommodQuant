namespace Commod
open System
module Units = 
    let getHours (d1:DateTime) (d2:DateTime) = (d2.AddDays(1.0)-d1).TotalHours * 1.0<h>
    let getHoursDST (d1:DateTime) (d2:DateTime) tz = 
        //let tz = TimeZoneInfo.FindSystemTimeZoneById("Central European Standard Time")
        let d1' = DateTime.SpecifyKind(d1, DateTimeKind.Unspecified)
        let d2' = DateTime.SpecifyKind(d2.AddDays(1.0), DateTimeKind.Unspecified)
        let d = TimeZoneInfo.ConvertTimeToUtc(d1', tz )
        let d' = TimeZoneInfo.ConvertTimeToUtc(d2', tz )
        (d'-d).TotalHours * 1.0<h>

    let convertMWtoMWHwDST (d1:DateTime) (d2:DateTime) (q:float<mw>) :float<mwh> = 
        let tz = TimeZoneInfo.FindSystemTimeZoneById("Central European Standard Time")
        q * (getHoursDST d1 d2 tz)

    let convertMWtoMWH (d1:DateTime) (d2:DateTime) (q:float<mw>) :float<mwh> = 
        q * (getHours d1 d2 )

    let convertMWHtoMMBTU (q:float<mwh>) = 
        q / 0.293071<mwh/mmbtu>
        
    let convertMTtoBBL (q:float<mt>) = //FO380
        q * 6.35<bbl/mt>


