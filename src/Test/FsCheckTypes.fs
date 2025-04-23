module FsCheckTypes

open FsCheck
open FsCheck.FSharp

type PositiveSmallFloat =
    static member Float() =
        ArbMap.defaults
        |> ArbMap.arbitrary<float>
        |> Arb.filter (fun t -> (t > 0.0) && (t < 100.0))

type ValidDate =
    static member DateTime() =
        ArbMap.defaults
        |> ArbMap.arbitrary<System.DateTime>
        |> Arb.filter (fun t -> (t.Year >= 1981) && (t.Year <= 2080))

//arb type to limit float random test to positive normal floats
type PositiveFloat =
    static member float() =
        ArbMap.defaults
        |> ArbMap.arbitrary<NormalFloat>
        |> Arb.convert (fun (NormalFloat x) -> x) NormalFloat
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
        |> Arb.convert BeginOfCalendarInt (fun (BeginOfCalendarInt i) -> i)

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
        |> Arb.convert MonthString (fun (MonthString s) -> s)

    static member Corr() =
        Gen.elements [ -0.99..0.03..0.99 ] |> Arb.fromGen |> Arb.convert Corr (fun (Corr f) -> f)


type IntLessThan100 =
    static member Int() =
        ArbMap.defaults
        |> ArbMap.arbitrary<int>
        |> Arb.filter (fun t -> (t >= 0) && (t < 100))

let inline near a b eps =
    (a - eps <= b && a + eps >= b) |> Prop.label (sprintf "%A vs %A with %A" a b eps)

let inline nearstr a b eps str =
    (a - eps <= b && a + eps >= b) |> Prop.label (sprintf "%s: %A vs %A with %A" str a b eps)
