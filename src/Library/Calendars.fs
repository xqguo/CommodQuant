namespace Commod
module Calendars = 
    open System
    open System.IO
    open Utils
    open Deedle
    open Deedle.Internal

    let ROOT = Path.GetDirectoryName( Reflection.Assembly.GetExecutingAssembly().Location)
    printfn "ROOT=%s" (ROOT +/ "test" +/ "another")

    let readCalendar f = 
        File.ReadAllLines( ROOT +/ "holidays" +/ f ) 
        |> Array.choose( parseDateExact "yyyyMMMdd" )
        |> set

    // let private pltCalendar = [ DateTime( 2019, 1, 1); DateTime(2019, 12, 25)] |> set
    // let private iceCalendar = [ DateTime( 2019, 1, 1)] |> set
    let private pltsgpCalendar = readCalendar "pltsgp.txt"
    let private pltldnCalendar = readCalendar "pltldn.txt"
    let private iceCalendar = readCalendar "ice.txt"
    let private nymCalendar = readCalendar "nym.txt"

    let calendars = 
        [ 
            (ICE, iceCalendar)
            (PLTSGP, pltsgpCalendar)
            (PLTLDN, pltldnCalendar)
            (USD, Set.empty)
        ] 
        |> Map.ofList

    let getCalendar ins (calendars:Map<_,_>) = 
        match ins with
        | DBRT -> [PLTLDN] |> set
        | JKM | FO380 | FO180 -> [PLTSGP] |> set
        | BRT | GO | TTF  -> [ICE] |> set 
        | NG | DUB | FO3_5 | JCC |SGO | SJET -> invalidOp "not implemented"
        |> Set.fold ( fun acc s -> Set.union acc calendars.[s] ) Set.empty


