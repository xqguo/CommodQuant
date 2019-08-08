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
            (CME, nymCalendar )
            (PLTSGP, pltsgpCalendar)
            (PLTLDN, pltldnCalendar)
            (USD, Set.empty)
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


