namespace Commod
[<AutoOpen>]
module DomainTypes  =
    open System
    open System.IO
    open Deedle
    open FSharp.Data
    open QLNet
    open FSharp.Reflection

    [<Measure>] type bbl
    [<Measure>] type mt 
    [<Measure>] type mmbtu 
    [<Measure>] type USD 
    [<Measure>] type GBP
    [<Measure>] type EUR
    [<Measure>] type lot

    let applyCaseDecimal<'a> (s:string) (v:decimal) = 
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> FSharpValue.MakeUnion(case,[| box v|]) :?> 'a
        |_ -> invalidOp <| sprintf "Unknown case %s" s

    //type DecimalUnit = 
    //    | USDBBL of decimal<USD/bbl> 
    //    | USDMT of decimal<USD/mt> 
    //    | USDMMBTU of decimal<USD/mmbtu> 
    //    | BBL of decimal<bbl> 
    //    | MT of decimal<mt> 
    //    | MMBTU of decimal<mmbtu> 
    //    | LOT of decimal<lot> 
    //    | USD of decimal<USD> 
    //    | EUR of decimal<EUR>
    //    static member applyCase (case:string) (x:decimal) =  
    //        applyCaseDecimal<DecimalUnit> case x 

    //let inline applyDecimalUnit (s:string) (v:decimal) = 
    //    match FSharpType.GetUnionCases typeof<DecimalUnit> |> Array.filter (fun case -> case.Name = s) with
    //    |[|case|] -> 
    //        match case.Name with 
    //        | "USDBBL" -> 1M<USD/bbl> 
    //        | "USDMT" ->  1M<USD/mt> 
    //        | "USDMMBTU" -> 1M<USD/mmbtu> 
    //        | "BBL" -> 1M<bbl> 
    //        | "MT" -> 1M<mt> 
    //        | "MMBTU" -> 1M<mmbtu> 
    //        | "LOT" -> 1M<lot> 
    //        | "USD" -> 1M<USD> 
    //        | "EUR" -> 1M<EUR>
    //    |_ -> invalidOp <| sprintf "Unknown case %s" s

    let getCaseDecimal (x:'a) = 
            match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, v -> 
                let v' = v |> Array.head :?> decimal 
                case.Name, v'

    let private map (p1:'a) (p2:'a) f = 
            let case1, v1 = getCaseDecimal p1
            let case2, v2 = getCaseDecimal p2
            if case1 = case2 then 
                f v1 v2 |> applyCaseDecimal case1
            else 
                invalidOp <| sprintf "Inconsistent cases %A %A" p1 p2

    type UnitPrice = 
        | USDBBL of decimal<USD/bbl> 
        | USDMT of decimal<USD/mt> 
        | USDMMBTU of decimal<USD/mmbtu> 
        static member applyCase (ccy:string) (x:decimal) =  
            applyCaseDecimal<UnitPrice> ccy x 
        static member (-) (p1:UnitPrice, p2:UnitPrice) = 
            map p1 p2 (-)
        static member (+) (p1:UnitPrice, p2:UnitPrice) = 
            map p1 p2 (-)

    type QuantityAmount = 
        | BBL of decimal<bbl> 
        | MT of decimal<mt> 
        | MMBTU of decimal<mmbtu> 
        | LOT of decimal<lot> 
        static member applyCase (ccy:string) (x:decimal) =  
            applyCaseDecimal<QuantityAmount> ccy x 
        static member (-) (p1:QuantityAmount, p2:QuantityAmount) = 
            map p1 p2 (-)
        static member (+) (p1:QuantityAmount, p2:QuantityAmount) = 
            map p1 p2 (-)

    type CurrencyAmount = 
        | USD of decimal<USD> 
        | EUR of decimal<EUR>
        static member applyCase (ccy:string) (x:decimal) =  
            applyCaseDecimal<CurrencyAmount> ccy x 
        static member (-) (p1:CurrencyAmount, p2:CurrencyAmount) = 
            map p1 p2 (-)
        static member (+) (p1:CurrencyAmount, p2:CurrencyAmount) = 
            map p1 p2 (-)

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
        member x.Lot = getCaseDecimal x.LotSize |> snd
         
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
    let ROOT = Path.GetDirectoryName( Reflection.Assembly.GetExecutingAssembly().Location)
