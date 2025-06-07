module TestGabillon

open FsCheck.FSharp
open Xunit
open FsCheck.Xunit
open Commod
open FsCheck
open FsCheckTypes
open MathNet.Numerics.LinearAlgebra

[<Property(Arbitrary = [| typeof<PositiveFloat> |])>]
let ``forward cross covariance equivalence to forward covariance`` tn Ti Tj sigmas1 sigmas2 sigmal k rho =
    let tm = 0.
    let tn = [ tn; Ti; Tj ] |> List.min

    let xv =
        fwdXCovariance tm tn Ti Tj sigmas1 sigmas2 sigmal sigmal k k 1.0 rho rho 1.0

    let v = fwdCovariance tm tn Ti Tj sigmas1 sigmas2 sigmal k rho
    near xv v 1E-12 |> Prop.label (sprintf "variance identical") 
    |> Prop.collect( xv - v )

[<Property(Arbitrary = [| typeof<PositiveFloat> |])>]
let ``forward cross covariance equivalence to forward variance`` tn Ti sigmas1 sigmal k rho =
    let tm = 0.
    let tn = [ tn; Ti ] |> List.min

    let xv =
        fwdXCovariance tm tn Ti Ti sigmas1 sigmas1 sigmal sigmal k k 1.0 rho rho 1.0

    let v = fwdVariance tm tn Ti sigmas1 sigmal k rho
    near xv v 1E-12 |> Prop.label (sprintf "variance identical") 
    |> Prop.collect( xv - v )

[<Property(Arbitrary = [| typeof<PositiveFloat> ; typeof<MyGenerator> |])>]
let ``forward cross covariance duality`` tn Ti Tj sigmas1 sigmas2 sigmal1 sigmal2 k1 k2 (Corr rho)=
    let tm = 0.
    let tn = [ tn; Ti; Tj ] |> List.min

    let v = fwdXCovariance tm tn Ti Tj sigmas1 sigmas2 sigmal1 sigmal2 k1 k2 rho rho rho rho
    let v' = fwdXCovariance tm tn Tj Ti sigmas2 sigmas1 sigmal2 sigmal1 k2 k1 rho rho rho rho

    near v' v 1E-12 |> Prop.label (sprintf "variance identical") 
    |> Prop.collect( v' - v )