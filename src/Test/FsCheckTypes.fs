module FsCheckTypes
open FsCheck

type SmallInt =
    static member Int() =
        Arb.Default.Int32()
        |> Arb.filter (fun t -> (t > 1) && (t < 11))

type PositiveSmallFloat = 
    static member Float() = 
        Arb.Default.Float()
        |> Arb.filter (fun t -> (t>0.0) && (t<100.))

type ValidDate = 
    static member Date() =
        Arb.Default.DateTime()
        |> Arb.filter (fun t -> (t.Year >= 1980) && (t.Year <=2080))

//arb type to limit float random test to positive normal floats 
type PositiveFloat = 
    static member float() =
        Arb.Default.NormalFloat()
        |> Arb.convert float NormalFloat
        |> Arb.mapFilter abs ( fun x  -> x > 0.0 )

type BeginOfCalenderInt =
    static member Int() =
        let sample = [0;1;3;6;12]|>Set
        Arb.Default.Int32()
        |> Arb.filter (fun t -> (sample.Contains t))

type MonthString = 
    static member String() =
        let month = ["Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"] |>Set
        Arb.Default.String()
        |> Arb.filter (fun t -> month.Contains t)

type IntLessThan100 = 
    static member Int()=
        Arb.Default.Int32()
        |> Arb.filter (fun t -> (t>=0) && (t<80))

