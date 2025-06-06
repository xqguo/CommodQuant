namespace Commod

[<AutoOpen>]
module DomainTypes =
    open System
    open FSharp.Data
    open QLNet
    open FSharp.Reflection

    /// <summary>Unit of measure for barrels.</summary>
    [<Measure>]
    type bbl

    /// <summary>Unit of measure for metric tons.</summary>
    [<Measure>]
    type mt //metric ton

    /// <summary>Unit of measure for million tons.</summary>
    [<Measure>]
    type mmt //million tons

    /// <summary>Unit of measure for million British Thermal Units.</summary>
    [<Measure>]
    type mmbtu

    /// <summary>Unit of measure for megawatts.</summary>
    [<Measure>]
    type mw

    /// <summary>Unit of measure for hours.</summary>
    [<Measure>]
    type h

    /// <summary>Unit of measure for days.</summary>
    [<Measure>]
    type d

    /// <summary>Unit of measure for months.</summary>
    [<Measure>]
    type m

    /// <summary>Unit of measure for feet.</summary>
    [<Measure>]
    type ft

    /// <summary>Unit of measure for megawatt-hours.</summary>
    [<Measure>]
    type mwh = mw * h

    /// <summary>Unit of measure for US Dollars.</summary>
    [<Measure>]
    type USD

    /// <summary>Unit of measure for Pound Sterling.</summary>
    [<Measure>]
    type GBP

    /// <summary>Unit of measure for Euros.</summary>
    [<Measure>]
    type EUR

    /// <summary>Unit of measure for lots (standard contract size).</summary>
    [<Measure>]
    type lot

    /// <summary>
    /// Creates a union case of a given type with a decimal value.
    /// </summary>
    /// <typeparam name="'a">The union type.</typeparam>
    /// <param name="s">The name of the union case.</param>
    /// <param name="v">The decimal value for the union case.</param>
    /// <returns>An instance of the union type 'a.</returns>
    /// <exception cref="InvalidOperationException">Thrown if the case name is not found in the union type.</exception>
    let applyCaseDecimal<'a> (s: string) (v: decimal) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> FSharpValue.MakeUnion(case, [| box v |]) :?> 'a
        | _ -> invalidOp <| sprintf "Unknown case %s" s

    /// <summary>
    /// Gets the case name and decimal value from a union instance.
    /// Assumes the union case holds a single decimal value.
    /// </summary>
    /// <typeparam name="'a">The union type.</typeparam>
    /// <param name="x">The union instance.</param>
    /// <returns>A tuple containing the case name (string) and the decimal value.</returns>
    let getCaseDecimal (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, v ->
            let v' = v |> Array.head :?> decimal
            case.Name, v'

    /// <summary>
    /// Applies a function to the decimal values of two union instances, if their cases match.
    /// </summary>
    /// <typeparam name="'a">The union type.</typeparam>
    /// <param name="p1">The first union instance.</param>
    /// <param name="p2">The second union instance.</param>
    /// <param name="f">The function to apply to the decimal values.</param>
    /// <returns>A new union instance of type 'a with the result of the function, under the same case.</returns>
    /// <exception cref="InvalidOperationException">Thrown if the cases of p1 and p2 do not match.</exception>
    let private map (p1: 'a) (p2: 'a) f =
        let case1, v1 = getCaseDecimal p1
        let case2, v2 = getCaseDecimal p2

        if case1 = case2 then
            f v1 v2 |> applyCaseDecimal<'a> case1
        else
            invalidOp <| sprintf "Inconsistent cases %A %A" p1 p2

    /// <summary>
    /// Applies a function to the decimal value of a union instance and another value.
    /// </summary>
    /// <typeparam name="'a">The union type.</typeparam>
    /// <typeparam name="'b">The type of the second value (will be converted to decimal).</typeparam>
    /// <param name="p1">The union instance.</param>
    /// <param name="v">The second value.</param>
    /// <param name="f">The function to apply.</param>
    /// <returns>A new union instance of type 'a with the result of the function, under the same case.</returns>
    let inline private mapDecimal (p1: 'a) (v: 'b) f =
        let case1, v1 = getCaseDecimal p1
        f v1 (decimal v) |> applyCaseDecimal<'a> case1

    /// <summary>
    /// Represents a quantity with a specific unit of measure (BBL, MT, MMBTU, LOT).
    /// Supports arithmetic operations and provides access to the value and case name.
    /// </summary>
    type QuantityAmount =
        | BBL of decimal<bbl>
        | MT of decimal<mt>
        | MMBTU of decimal<mmbtu>
        | LOT of decimal<lot>

        /// <summary>Creates a QuantityAmount instance from a case name string and a decimal value.</summary>
        static member applyCase (ccy: string) (x: decimal) = applyCaseDecimal<QuantityAmount> ccy x
        /// <summary>Subtracts two QuantityAmount instances if their cases match.</summary>
        static member (-)(p1: QuantityAmount, p2: QuantityAmount) = map p1 p2 (-)
        /// <summary>Adds two QuantityAmount instances if their cases match.</summary>
        static member (+)(p1: QuantityAmount, p2: QuantityAmount) = map p1 p2 (+)
        /// <summary>Multiplies a QuantityAmount by a decimal value.</summary>
        static member (*)(p1: QuantityAmount, v: decimal) = mapDecimal p1 v (*)
        /// <summary>Divides a QuantityAmount by a decimal value.</summary>
        static member (/)(p1: QuantityAmount, v: decimal) = mapDecimal p1 v (/)
        /// <summary>Multiplies a QuantityAmount by an integer value.</summary>
        static member (*)(p1: QuantityAmount, v: int) = mapDecimal p1 v (*)
        /// <summary>Divides a QuantityAmount by an integer value.</summary>
        static member (/)(p1: QuantityAmount, v: int) = mapDecimal p1 v (/)
        /// <summary>Gets the underlying decimal value of the QuantityAmount.</summary>
        member x.Value = getCaseDecimal x |> snd
        /// <summary>Gets the case name (e.g., "BBL", "MT") of the QuantityAmount.</summary>
        member x.Case = getCaseDecimal x |> fst

    /// <summary>
    /// Represents a currency amount with a specific unit (USD, EUR).
    /// Supports arithmetic operations and provides access to the value and case name.
    /// </summary>
    type CurrencyAmount =
        | USD of decimal<USD>
        | EUR of decimal<EUR>

        /// <summary>Creates a CurrencyAmount instance from a case name string and a decimal value.</summary>
        static member applyCase (ccy: string) (x: decimal) = applyCaseDecimal<CurrencyAmount> ccy x
        /// <summary>Subtracts two CurrencyAmount instances if their cases match.</summary>
        static member (-)(p1: CurrencyAmount, p2: CurrencyAmount) = map p1 p2 (-)
        /// <summary>Adds two CurrencyAmount instances if their cases match.</summary>
        static member (+)(p1: CurrencyAmount, p2: CurrencyAmount) = map p1 p2 (+)
        /// <summary>Gets the underlying decimal value of the CurrencyAmount.</summary>
        member x.Value = getCaseDecimal x |> snd
        /// <summary>Gets the case name (e.g., "USD", "EUR") of the CurrencyAmount.</summary>
        member x.Case = getCaseDecimal x |> fst

    /// <summary>
    /// Represents a unit price, combining a currency and a quantity unit (e.g., USD/bbl).
    /// Supports arithmetic operations and multiplication/division with QuantityAmount.
    /// </summary>
    type UnitPrice =
        | USDBBL of decimal<USD / bbl>
        | USDMT of decimal<USD / mt>
        | USDMMBTU of decimal<USD / mmbtu>

        /// <summary>Creates a UnitPrice instance from a case name string and a decimal value.</summary>
        static member applyCase (ccy: string) (x: decimal) = applyCaseDecimal<UnitPrice> ccy x
        /// <summary>Gets the underlying decimal value of the UnitPrice.</summary>
        member x.Value = getCaseDecimal x |> snd
        /// <summary>Gets the case name (e.g., "USDBBL") of the UnitPrice.</summary>
        member x.Case = getCaseDecimal x |> fst
        /// <summary>Gets the quantity part of the case name (e.g., "BBL" from "USDBBL").</summary>
        member x.QtyCase = x.Case.Substring 3
        /// <summary>Gets the currency part of the case name (e.g., "USD" from "USDBBL").</summary>
        member x.CcyCase = x.Case.Substring(0, 3)
        /// <summary>Subtracts two UnitPrice instances if their cases match.</summary>
        static member (-)(p1: UnitPrice, p2: UnitPrice) = map p1 p2 (-)
        /// <summary>Adds two UnitPrice instances if their cases match.</summary>
        static member (+)(p1: UnitPrice, p2: UnitPrice) = map p1 p2 (+)
        /// <summary>Multiplies a UnitPrice by a decimal value.</summary>
        static member (*)(p1: UnitPrice, v: decimal) = mapDecimal p1 v (*)
        /// <summary>Divides a UnitPrice by a decimal value.</summary>
        static member (/)(p1: UnitPrice, v: decimal) = mapDecimal p1 v (/)
        /// <summary>Multiplies a UnitPrice by an integer value.</summary>
        static member (*)(p1: UnitPrice, v: int) = mapDecimal p1 v (*)
        /// <summary>Divides a UnitPrice by an integer value.</summary>
        static member (/)(p1: UnitPrice, v: int) = mapDecimal p1 v (/)

        /// <summary>Multiplies a UnitPrice by a QuantityAmount, resulting in a CurrencyAmount if units match.</summary>
        /// <exception cref="InvalidOperationException">Thrown if quantity units do not match.</exception>
        static member (*)(p: UnitPrice, v: QuantityAmount) =
            if p.QtyCase = v.Case then
                p.Value * v.Value |> CurrencyAmount.applyCase p.CcyCase
            else
                invalidOp "Mismatch qty unit"

        /// <summary>Multiplies a QuantityAmount by a UnitPrice, resulting in a CurrencyAmount if units match.</summary>
        /// <exception cref="InvalidOperationException">Thrown if quantity units do not match.</exception>
        static member (*)(v: QuantityAmount, p: UnitPrice) =
            if p.QtyCase = v.Case then
                p.Value * v.Value |> CurrencyAmount.applyCase p.CcyCase
            else
                invalidOp "Mismatch qty unit"

    /// <summary>
    /// Calculates the average price from a sequence of UnitPrice instances.
    /// Assumes all UnitPrice instances share the same case (currency and quantity unit).
    /// </summary>
    /// <param name="s">A sequence of UnitPrice instances.</param>
    /// <returns>A UnitPrice representing the average price.</returns>
    /// <exception cref="System.ArgumentException">Thrown if the input sequence is empty.</exception>
    let avgPrice (s: seq<UnitPrice>) =
        let count = Seq.length s
        let total = s |> Seq.reduce (+)
        total / (decimal count) // Ensure division is by decimal for UnitPrice operations

    /// <summary>
    /// Represents holiday calendar codes used for date adjustments.
    /// </summary>
    type HolidayCode =
        | PLTSGP // Platts Singapore
        | PLTLDN // Platts London
        | ICE    // Intercontinental Exchange
        | CME    // Chicago Mercantile Exchange
        | UK     // United Kingdom general holidays
        | UKB    // United Kingdom banking holidays (potentially different from general)
        | ALLDAYS // Represents a calendar with no holidays (every day is a business day)

    /// <summary>
    /// Represents various commodity instruments.
    /// </summary>
    type Instrument =
        | DBRT   // Dated Brent crude oil
        | BRT    // Brent crude oil futures
        | GO     // Gas Oil
        | FO     // Fuel Oil (generic)
        | FO380  // Fuel Oil 380cst
        | FO180  // Fuel Oil 180cst
        | FO35   // Fuel Oil 3.5% Sulfur Barges
        | MFO    // Marine Fuel Oil 0.5% FOB Singapore
        | JKM    // Japan Korea Marker (LNG)
        | JCC    // Japan Crude Cocktail
        | TTF    // Dutch TTF Gas (converted USD/mmbtu composite price)
        | SGO    // Singapore Gas Oil
        | NG     // Henry Hub Natural Gas
        | NBP    // UK National Balancing Point Natural Gas
        | SPP    // Spain Power
        | DUB    // Dubai Crude Oil
        | SJET   // Singapore Jet Fuel
        | API2   // API2 Coal Index

    /// <summary>
    /// Represents contract dates, typically storing future and option expiration dates for various pillars.
    /// The map key is a pillar string (e.g., "Jan24"), and the value is a tuple of (FutureExpiry, OptionExpiry).
    /// </summary>
    type ContractDates =
        | ContractDates of Map<string, DateTime * DateTime>

        /// <summary>Gets the future expiration date for a given pillar string.</summary>
        member this.Item s =
            let (ContractDates c) = this
            c.Item s |> fst

        /// <summary>Gets a map of pillar strings to future expiration dates.</summary>
        member this.Fut =
            let (ContractDates c) = this
            c |> Map.map (fun _ v -> fst v)

        /// <summary>Gets a map of pillar strings to option expiration dates.</summary>
        member this.Opt =
            let (ContractDates c) = this
            c |> Map.map (fun _ v -> snd v)

    /// <summary>
    /// Represents a commodity definition, including its instrument type, calendar, contract dates,
    /// quotation unit, and lot size.
    /// </summary>
    type Commod =
        { /// <summary>The type of commodity instrument.</summary>
          Instrument: Instrument
          /// <summary>A set of holiday dates for this commodity's calendar.</summary>
          Calendar: Set<DateTime>
          /// <summary>Contract dates (future and option expiries) for this commodity.</summary>
          Contracts: ContractDates
          /// <summary>The standard unit price quotation for this commodity (e.g., USD/bbl).</summary>
          Quotation: UnitPrice
          /// <summary>The standard lot size for one contract of this commodity.</summary>
          LotSize: QuantityAmount }

        /// <summary>Gets the decimal value of the lot size.</summary>
        member x.Lot = getCaseDecimal x.LotSize |> snd

    /// <summary>Type provider for reading CSV files with "PILLAR,PRICE" schema.</summary>
    type PriceCsv = CsvProvider<"PILLAR,PRICE", Schema="string,decimal">
    /// <summary>Type provider for reading CSV files with contract dates, e.g., "Oct19,2019-08-27". Assumes no headers.</summary>
    type ContractCsv = CsvProvider<"Oct19,2019-08-27", HasHeaders=false, Schema="string,date">

    /// <summary>
    /// Represents a price curve as a map of pillar strings to UnitPrice instances.
    /// </summary>
    type PriceCurve =
        | PriceCurve of Map<string, UnitPrice>

        /// <summary>Gets a set of all pillar strings present in the curve.</summary>
        member this.Pillars =
            let (PriceCurve c) = this
            c |> Map.toSeq |> Seq.map (fst) |> set

        /// <summary>Gets the UnitPrice for a given pillar string.</summary>
        member this.Item s =
            let (PriceCurve c) = this
            c.Item s

        /// <summary>Creates a new PriceCurve with all prices set to a flat value, preserving original cases.</summary>
        member this.flatten s =
            let (PriceCurve c) = this
            c |> Map.map (fun _ v -> UnitPrice.applyCase v.Case s) |> PriceCurve

        /// <summary>Creates a new PriceCurve by shifting all prices by a given decimal amount.</summary>
        member this.shift s =
            let (PriceCurve c) = this
            c |> Map.map (fun _ v -> UnitPrice.applyCase v.Case (v.Value + s)) |> PriceCurve

        /// <summary>Gets an array of (pillarDate, priceValue) tuples, sorted by pillar date.</summary>
        member this.Observations =
            let (PriceCurve c) = this

            c
            |> Map.map (fun k v -> v.Value)
            |> Map.toArray
            |> Array.sortBy (fst >> pillarToDate)

    /// <summary>
    /// Represents volatility, either as a percentage or an absolute decimal value.
    /// </summary>
    type Vol =
        | PercentVol of decimal //percentage vol e.g. 20
        | AbsoluteVol of decimal // e.g. 0.2

    /// <summary>
    /// Represents a volatility curve as a map of pillar strings to Vol instances.
    /// </summary>
    type VolCurve =
        | VolCurve of Map<string, Vol>

        /// <summary>Gets a set of all pillar strings present in the curve.</summary>
        member this.Pillars =
            let (VolCurve c) = this
            c |> Map.toSeq |> Seq.map (fst) |> set

        /// <summary>
        /// Gets the volatility for a given pillar string, always returned as an absolute decimal value.
        /// If stored as PercentVol, it's converted (e.g., 20 becomes 0.20).
        /// </summary>
        member this.Item s =
            let (VolCurve c) = this

            match c.Item s with
            | PercentVol v -> v / 100M
            | AbsoluteVol v -> v

        /// <summary>Creates a new VolCurve with all volatilities set to a flat absolute value.</summary>
        member this.flatten s =
            let (VolCurve c) = this
            c |> Map.map (fun _ _ -> AbsoluteVol s) |> VolCurve

        /// <summary>Creates a new VolCurve by shifting all absolute volatilities by a given decimal amount.</summary>
        member this.shift s =
            let (VolCurve c) = this
            c |> Map.map (fun k v -> AbsoluteVol(this.Item k + s)) |> VolCurve // Uses .Item to get absolute before adding

        /// <summary>Gets an array of (pillarDate, absoluteVolatilityValue) tuples, sorted by pillar date.</summary>
        member this.Observations =
            let (VolCurve c) = this

            c
            |> Map.map (fun k v -> this.Item k) // Uses .Item to get absolute vol
            |> Map.toArray
            |> Array.sortBy (fst >> pillarToDate)

    /// <summary>
    /// Represents interest rate curves, currently supporting USD OIS (Overnight Index Swap) curves.
    /// </summary>
    type RateCurves = USDOIS of PiecewiseYieldCurve

    /// <summary>
    /// Represents a specific future contract instance.
    /// </summary>
    type FutureContract =
        { /// <summary>The underlying commodity definition.</summary>
          Fut: Commod
          /// <summary>The contract month pillar string (e.g., "Jan24").</summary>
          ContractMonth: string
          /// <summary>The quantity of the contract in lots.</summary>
          Quantity: decimal<lot>
          /// <summary>The fixed price of the contract.</summary>
          FixedPrice: UnitPrice }

    /// <summary>
    /// Defines a function type for pricing a future contract.
    /// Takes a FutureContract and a PriceCurve, returns a CurrencyAmount.
    /// </summary>
    type FutureContractPricer = FutureContract -> PriceCurve -> CurrencyAmount

    /// <summary>
    /// Represents a volatility smile/skew across different deltas for various pillars.
    /// </summary>
    /// <param name="pillars">Array of pillar strings (e.g., contract months).</param>
    /// <param name="deltas">Array of delta values (e.g., 0.10, 0.25, 0.50, 0.75, 0.90 for 10D, 25D, ATM, 75D, 90D calls).</param>
    /// <param name="vols">A 2D array where `vols[i][j]` is the volatility for `pillars[i]` at `deltas[j]`.</param>
    type VolDeltaSmile(pillars: string[], deltas: float[], vols: float[][]) =
        /// <summary>Gets the array of pillar strings.</summary>
        member val Pillars = pillars
        /// <summary>Gets the array of delta values.</summary>
        member val Deltas = deltas
        /// <summary>Gets the 2D array of volatility values.</summary>
        member val Vols = vols

        /// <summary>Gets the array of volatilities for a specific pillar string.</summary>
        member this.Item p =
            let i = Array.findIndex (fun x -> x = p) this.Pillars
            this.Vols.[i]

        /// <summary>Creates a new VolDeltaSmile by shifting all volatility values by a given amount.</summary>
        member this.Shift(x: float) =
            let vols' = vols |> Array.map (fun v -> v |> Array.map (fun s -> s + x))
            new VolDeltaSmile(this.Pillars, this.Deltas, vols')

    /// <summary>
    /// Specifies the payoff type of an option.
    /// </summary>
    type Payoff =
        | Call
        | Put
