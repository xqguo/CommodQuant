module FsCheckTypes

open FsCheck

type PositiveSmallFloat =
    static member Float() =
        Arb.Default.Float() |> Arb.filter (fun t -> (t > 0.0) && (t < 100.))

type ValidDate =
    static member DateTime() =
        Arb.Default.DateTime()
        |> Arb.filter (fun t -> (t.Year >= 1980) && (t.Year <= 2080))

//arb type to limit float random test to positive normal floats
type PositiveFloat =
    static member float() =
        Arb.Default.NormalFloat()
        |> Arb.convert float NormalFloat
        |> Arb.mapFilter abs (fun x -> x > 0.0)

type BeginOfCalendarInt =
    | BeginOfCalendarInt of int

    static member op_Explicit(BeginOfCalendarInt i) = i

type MonthString =
    | MonthString of string

    static member op_Explicit(MonthString i) = i

type Corr =
    | Corr of float

    static member op_Explicit(Corr i) = i

type MyGenerator =
    static member BeginOfCalenderInt() =
        Gen.elements [ 0; 1; 3; 6; 12 ]
        |> Arb.fromGen
        |> Arb.convert BeginOfCalendarInt int

    static member MonthString() =
        Gen.elements
            [ "Jan"
              "Feb"
              "Mar"
              "Apr"
              "May"
              "Jun"
              "Jul"
              "Aug"
              "Sep"
              "Oct"
              "Nov"
              "Dec" ]
        |> Arb.fromGen
        |> Arb.convert MonthString string

    static member Corr() =
        Gen.elements [ -0.99..0.03..0.99 ] |> Arb.fromGen |> Arb.convert Corr float


type IntLessThan100 =
    static member Int() =
        Arb.Default.Int32() |> Arb.filter (fun t -> (t >= 0) && (t < 100))

let inline near a b eps =
    (a - eps <= b && a + eps >= b) |@ sprintf "%A vs %A with %A" a b eps

let inline nearstr a b eps str =
    (a - eps <= b && a + eps >= b) |@ sprintf "%s: %A vs %A with %A" str a b eps
