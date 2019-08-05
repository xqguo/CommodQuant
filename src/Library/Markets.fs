namespace Commod
module Markets = 
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
        | _     -> Set.empty
        |> Set.fold ( fun acc s -> Set.union acc calendars.[s] ) Set.empty

    let brtDates = Frame.ReadCsv<string>(ROOT +/ "holidays" +/ "BrentPillars.csv", indexCol = "Month", inferTypes = false)
    let brtContracts = 
        brtDates.Columns?("Last Trade").As<string>()
        |> Series.filter( fun s v -> s <> "" && v <> "" )
        |> Series.mapValues parseMMddyy 
        |> ContractDates
        
    //GO contracts: https://www.theice.com/products/34361119/Low-Sulphur-Gasoil-Future
    //compute Last Trading Day: Trading shall cease at 12:00 hours, 2 business days prior to the 14th calendar day of the delivery.
    let goContracts = 
        ///start from current month last year
        let td = DateTime.Today |> dateAdjust' "-1ya" 
        let goHol = getCalendar GO calendars
        generateMonth (td |> dateAdjust' "a" ) true 
        |> Seq.map ( fun x -> ( x.ToString("MMM-yy"), x |> dateAdjust goHol "13d-2b" ))
        |> Seq.skipWhile( fun (_,d) -> d < td )
        |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
        |> Series.ofObservations
        |> ContractDates

    ///jkm contracts expiration is 15th of m-1, or previous bd. 
    let jkmContracts = 
        ///start from current month last year
        let td = DateTime.Today |> dateAdjust' "-1ya" 
        let jkmHol = getCalendar JKM calendars
        generateMonth (td |> dateAdjust' "a" ) true 
        |> Seq.map ( fun x -> ( x.ToString("MMM-yy"), x |> dateAdjust jkmHol "-1m15d-1b" ))
        |> Seq.skipWhile( fun (_,d) -> d < td )
        |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
        |> Series.ofObservations
        |> ContractDates

    let getJkmPeriod x = 
        let (d0, d1 ) = getPeriod x 
        let jkmHol = getCalendar JKM calendars
        let d0' = dateAdjust jkmHol "-2ma15d-1b+1d" d0
        let d1' = dateAdjust jkmHol "-1ma15d-1b" d1
        (d0', d1')

    let jccContracts = 
        ///start from current month last year
        let td = DateTime.Today |> dateAdjust' "-1ya" 
        generateMonth (td |> dateAdjust' "a" ) true 
        |> Seq.map ( fun x -> ( x.ToString("MMM-yy"), x ))
        |> Seq.filter( fun (_,d) -> d >= td )
        |> Seq.take 24
        |> Series.ofObservations
        |> ContractDates

    /// For jcc underlying vol fixing dates, lag by 1 month.
    let getJccVolPeriod x = 
        let (d0, d1 ) = getPeriod x 
        let d0' = dateAdjust' "-1ma" d0
        let d1' = dateAdjust' "-1me" d1
        (d0', d1')    

    let ttfContracts = 
        ///start from current month last year
        let td = DateTime.Today |> dateAdjust' "-1ya" 
        //https://www.theice.com/products/27996665/Dutch-TTF-Gas-Futures/
        //Expiration Date
        //Trading will cease at the close of business two Business Days prior to the first calendar day of the delivery month, quarter, season, or calendar.
        let getExp d = dateAdjust (getCalendar TTF calendars ) "-2b" d
        generateMonth (td |> dateAdjust' "a" ) true 
        |> Seq.map ( fun x -> ( x.ToString("MMM-yy"), (getExp x)  ))
        |> Seq.skipWhile( fun (_,d) -> d < td )
        |> Seq.takeWhile( fun( _,d) -> d.Year < 2041 )
        |> Series.ofObservations
        |> ContractDates

    let getContracts ins =
        match ins with
        | BRT -> brtContracts
        | JKM -> jkmContracts
        | JCC -> jccContracts
        | TTF -> ttfContracts
        | _ -> //dummy example 
            [ ("Mar-19", DateTime(2019, 03, 31)) ; ("Apr-19", DateTime(2019, 4, 30))]
            |> series |> ContractDates

    let getCommod q lotsize ins = 
        let cals = getCalendar ins calendars
        let contracts = getContracts ins    
        { Calendar = cals; Contracts = contracts; Quotation = q; LotSize = lotsize}  

    //type PriceCurve<[<Measure>]'u> = PriceCurve of Series<string, float<'u>> //prices with quotation
    // these depends on the data format, as in PriceVols.csv
    let getPrices (df:Frame<string,string>) ins = 
        let (ContractDates c ) = getContracts ins
        let tag = ins.ToString()
        df?(tag)
        |> Series.filter( fun k _ -> c.ContainsKey k)

    let applyUSDbbl s = s |> Series.mapValues (fun v -> v * 1.<USD/bbl>)
    let applyUSDmmbtu s = s |> Series.mapValues (fun v -> v * 1.<USD/mmbtu>)
    let inline removeMeasure s = s |> Series.mapValues float

    let getBrtPrices df = getPrices df BRT |> applyUSDbbl
    let getJccPrices df = getPrices df JCC |> applyUSDbbl
    let getJkmPrices df = getPrices df JKM |> applyUSDmmbtu
    let getTtfPrices df = getPrices df TTF |> applyUSDmmbtu

    ///assuming raw data unit is in %. 
    let getVols (df:Frame<string,string>) ins = 
        let (ContractDates c ) = getContracts ins
        let tag = ins.ToString() + " Vol"
        df?(tag)
        |> Series.filter( fun k _ -> c.ContainsKey k)
        |> Series.mapValues( fun v -> v /100.)

    let filterCurve p pillars = 
        p |> Series.filter( fun k _ -> Set.contains k pillars)

    let shiftCurve p (v:seq<float>) = 
        ((Series.keys p), p.Values, v ) |||> Seq.map3( fun p v0 v1 -> (p, (v0 + v1 ))) |> series

    let inline overrideCurve p (v:seq<float>) = 
        ((Series.keys p), p.Values, v ) |||> Seq.map3( fun p v0 v1 -> (p, ((v0 + 1.0<_> - v0 )* v1 ))) |> series

    //these depends on the data format, as in PriceFixes.csv
    //let getFixes (df:Frame<DateTime,string>) ins = 
    //     let tag = ins.ToString()
    //     df?(tag)
    // let getBrtFixes df = getFixes df BRT |> applyUSDbbl
    // let getJccFixes df = getFixes df JCC |> applyUSDbbl
    // let getJkmFixes df = getFixes df JKM |> applyUSDmmbtu