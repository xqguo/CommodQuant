namespace Commod
[<AutoOpen>]
module DomainTypes  =
    open System
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

    let getCaseDecimal (x:'a) = 
            match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, v -> 
                let v' = v |> Array.head :?> decimal 
                case.Name, v'

    let private map (p1:'a) (p2:'a) f = 
            let case1, v1 = getCaseDecimal p1
            let case2, v2 = getCaseDecimal p2
            if case1 = case2 then 
                f v1 v2 |> applyCaseDecimal<'a> case1
            else 
                invalidOp <| sprintf "Inconsistent cases %A %A" p1 p2

    let inline private mapDecimal (p1:'a) (v:'b) f = 
            let case1, v1 = getCaseDecimal p1
            f v1 (decimal v)
            |> applyCaseDecimal<'a> case1


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
            map p1 p2 (+)
        static member (*) (p1:QuantityAmount, v:decimal) = 
            mapDecimal p1 v (*)
        static member (/) (p1:QuantityAmount, v:decimal) = 
            mapDecimal p1 v (/)
        static member (*) (p1:QuantityAmount, v:int) = 
            mapDecimal p1 v (*)
        static member (/) (p1:QuantityAmount, v:int) = 
            mapDecimal p1 v (/)
        member x.Value =             
            getCaseDecimal x |> snd
        member x.Case = 
            getCaseDecimal x |> fst

    type CurrencyAmount = 
        | USD of decimal<USD> 
        | EUR of decimal<EUR>
        static member applyCase (ccy:string) (x:decimal) =  
            applyCaseDecimal<CurrencyAmount> ccy x 
        static member (-) (p1:CurrencyAmount, p2:CurrencyAmount) = 
            map p1 p2 (-)
        static member (+) (p1:CurrencyAmount, p2:CurrencyAmount) = 
            map p1 p2 (+)
        member x.Value =             
            getCaseDecimal x |> snd
        member x.Case = 
            getCaseDecimal x |> fst

    type UnitPrice = 
        | USDBBL of decimal<USD/bbl> 
        | USDMT of decimal<USD/mt> 
        | USDMMBTU of decimal<USD/mmbtu> 
        static member applyCase (ccy:string) (x:decimal) =  
            applyCaseDecimal<UnitPrice> ccy x 
        member x.Value =             
            getCaseDecimal x |> snd
        member x.Case = 
            getCaseDecimal x |> fst
        member x.QtyCase = 
            x.Case.Substring 3 
        member x.CcyCase = 
            x.Case.Substring(0,3)
        static member (-) (p1:UnitPrice, p2:UnitPrice) = 
            map p1 p2 (-)
        static member (+) (p1:UnitPrice, p2:UnitPrice) = 
            map p1 p2 (+)
        static member (*) (p1:UnitPrice, v:decimal) = 
            mapDecimal p1 v (*)
        static member (/) (p1:UnitPrice, v:decimal) = 
            mapDecimal p1 v (/)
        static member (*) (p1:UnitPrice, v:int) = 
            mapDecimal p1 v (*)
        static member (/) (p1:UnitPrice, v:int) = 
            mapDecimal p1 v (/)
        static member (*) (p:UnitPrice,v:QuantityAmount) = 
            if p.QtyCase = v.Case then 
                p.Value * v.Value |> CurrencyAmount.applyCase p.CcyCase
            else 
                invalidOp "Mismatch qty unit"
        static member (*) (v:QuantityAmount, p:UnitPrice ) = 
            if p.QtyCase = v.Case then 
                p.Value * v.Value |> CurrencyAmount.applyCase p.CcyCase
            else 
                invalidOp "Mismatch qty unit"

    let avgPrice (s:seq<UnitPrice>) = 
        let count = Seq.length s
        let total = s |> Seq.reduce (+) 
        total / count

    type HolidayCode = 
        | PLTSGP 
        | PLTLDN 
        | ICE
        | CME
        | UK
        | UKB
        | ALLDAYS
    
    type Instrument = //full list of known instruments
        | DBRT //dated brent
        | BRT
        | GO
        | FO380
        | FO180
        | FO35 //Fuel oil 3.5 Barges 
        | MFO //Marine Fuel Oil 0.5% FOB Singapore
        | JKM
        | JCC
        | TTF //converted USD/mmbtu compo price
        | SGO //Singapore Gas oil ref...
        | NG // Herry Hub natural gas
        | NBP // NBP natural gas
        | DUB // dubai crude
        | SJET // Sing Jet

    type ContractDates = 
        ContractDates of Map<string, DateTime*DateTime> with //how to interprete tenor code to date
        member this.Item s = 
            let (ContractDates c) = this
            c.Item s |> fst
        member this.Fut = 
            let (ContractDates c) = this
            c |> Map.map( fun _ v -> fst v )
        member this.Opt = 
            let (ContractDates c) = this
            c |> Map.map( fun _ v -> snd v )

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

    type PriceCurve = 
        PriceCurve of Map<string, UnitPrice> with//prices with quotation
        member this.Pillars = 
            let (PriceCurve c) = this
            c |> Map.toSeq |> Seq.map( fst ) |> set
        member this.Item s = 
            let (PriceCurve c) = this
            c.Item s
        member this.flatten s = 
            let (PriceCurve c) = this
            c |> Map.map( fun _ v -> UnitPrice.applyCase v.Case s) |> PriceCurve
        member this.shift s = 
            let (PriceCurve c) = this
            c |> Map.map( fun _ v -> UnitPrice.applyCase v.Case (v.Value + s)) |> PriceCurve

    type Vol = 
        | PercentVol of decimal //percentage vol e.g. 20
        | AbsoluteVol of decimal // e.g. 0.2

    type VolCurve = 
        VolCurve of Map<string, Vol > with//prices with quotation
        member this.Pillars = 
            let (VolCurve c) = this
            c |> Map.toSeq |> Seq.map( fst ) |> set
        member this.Item s = 
            let (VolCurve c) = this
            match c.Item s with //always return absolute vol
            | PercentVol v -> v / 100M
            | AbsoluteVol v -> v
        member this.flatten s = 
            let (VolCurve c) = this
            c |> Map.map( fun _ _ -> AbsoluteVol s) |> VolCurve
        member this.shift s = 
            let (VolCurve c) = this
            c |> Map.map( fun k v -> AbsoluteVol ( this.Item k  + s )) |> VolCurve

    type RateCurves = 
        | USDOIS of PiecewiseYieldCurve

    type FutureContract = 
        { 
            Fut:Commod
            ContractMonth: string
            Quantity: decimal<lot>
            FixedPrice: UnitPrice
        }

    type FutureContractPricer = FutureContract -> PriceCurve -> CurrencyAmount//function types

    type VolDeltaSmile ( pillars:string[], deltas:float[], vols:float[][]) =
        member val Pillars = pillars
        member val Deltas = deltas
        member val Vols = vols
        member this.Shift (x:float) = 
            let vols' = vols |> Array.map( fun v -> v |> Array.map (fun s -> s + x ))
            new VolDeltaSmile( this.Pillars, this.Deltas, vols')

    type Payoff = 
        | Call 
        | Put 
            
