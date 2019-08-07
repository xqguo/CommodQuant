namespace Commod
[<AutoOpen>]
module DomainTypes  =
    open System
    open System.IO
    open Deedle
    open FSharp.Data
    open QLNet

    let ROOT = Path.GetDirectoryName( Reflection.Assembly.GetExecutingAssembly().Location)

    [<Measure>] type bbl
    [<Measure>] type mt 
    [<Measure>] type mmbtu 
    [<Measure>] type USD 
    [<Measure>] type GBP
    [<Measure>] type EUR
    [<Measure>] type lot
    //[<Measure>] type point //percentage point for interest rates

    type HolidayCode = 
        | PLTSGP 
        | PLTLDN 
        | ICE
        | USD
    
    type ContractDates = ContractDates of Series<string,DateTime> //how to interprete tenor code to date

    type Commod<[<Measure>] 'c, [<Measure>] 'u>  = 
        { 
          Calendar : Set<DateTime> 
          Contracts: ContractDates
          Quotation: float<'c/'u> //e.g USD/bbl
          LotSize: float<'u/lot> //defined native unit and lot size, e.g. 1000.0<bbl/lot> 
        }
        member x.Lot = float x.LotSize //example of member functions

    type Instrument = //full list of known instruments
        | DBRT //dated brent
        | BRT
        | GO
        | FO380
        | FO180
        | FO3_5 //Fuel oil 3.5 Barges 
        | JKM
        | JCC
        | TTF //converted USD/mmbut compo price
        | SGO //Singapore Gas oil ref...
        | NG // Herry Hub natural gas
        | DUB // dubai crude
        | SJET // Sing Jet
         
    type PriceCsv = CsvProvider<"PILLAR,PRICE">

    type PriceCurve<[<Measure>]'u> = PriceCurve of Series<string, float<'u>> //prices with quotation

    type RateCurves = 
        | USDOIS of PiecewiseYieldCurve
