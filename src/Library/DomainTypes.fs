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
    type UnitPrice = 
        | USDBBL of decimal<USD/bbl> 
        | USDMT of decimal<USD/mt> 
        | USDMMBTU of decimal<USD/mmbtu> 

    type QuantityAmount = 
        | BBL of decimal<bbl> 
        | MT of decimal<mt> 
        | MMBTU of decimal<mmbtu> 
        | LOT of decimal<lot> 

    type CurrencyAmount = 
        | USD of decimal<USD> 
        | EUR of decimal<EUR> 

    type HolidayCode = 
        | PLTSGP 
        | PLTLDN 
        | ICE
        | CME
    
    type Instrument = //full list of known instruments
        | DBRT //dated brent
        | BRT
        | GO
        | FO380
        | FO180
        | FO3_5 //Fuel oil 3.5 Barges 
        | JKM
        | JCC
        | TTF //converted USD/mmbtu compo price
        | SGO //Singapore Gas oil ref...
        | NG // Herry Hub natural gas
        | DUB // dubai crude
        | SJET // Sing Jet

    type ContractDates = ContractDates of Series<string,DateTime> //how to interprete tenor code to date

    type Commod =
        { 
          Instrument: Instrument
          Calendar : Set<DateTime> 
          Contracts: ContractDates
          Quotation: UnitPrice //e.g USD/bbl
          LotSize: QuantityAmount //defined native unit 1000.0 bbl 
        }
        member x.Lot = 
            match x.LotSize with//example of member functions
            | BBL v -> decimal v
            | MT v -> decimal v

    let pair = ("carrot", "orange")
    let pair2 = ("apple", "red")

    // Use dict with Key-Value pairs already created.
    let fruit = dict[pair; pair2]
         
    type PriceCsv = CsvProvider<"PILLAR,PRICE", Schema="string,decimal">
    type ContractCsv = CsvProvider<"Oct19,2019-08-27", HasHeaders = false, Schema="string,date">

    type PriceCurve = PriceCurve of Series<string, UnitPrice> //prices with quotation

    type RateCurves = 
        | USDOIS of PiecewiseYieldCurve

    type FutureContract = 
        { 
            fut:Commod
            ContractMonth: string
            quantity: decimal<lot>
            fixedPrice: UnitPrice
        }

    type FutureContractPricer = FutureContract -> PriceCurve -> CurrencyAmount//function types
