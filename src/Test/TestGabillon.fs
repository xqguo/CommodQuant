module TestGabillon

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
    near xv v 1E-6 |@ sprintf "variance identical"

[<Property(Arbitrary = [| typeof<PositiveFloat> |])>]
let ``forward cross covariance equivalence to forward variance`` tn Ti sigmas1 sigmal k rho =
    let tm = 0.
    let tn = [ tn; Ti ] |> List.min

    let xv =
        fwdXCovariance tm tn Ti Ti sigmas1 sigmas1 sigmal sigmal k k 1.0 rho rho 1.0

    let v = fwdVariance tm tn Ti sigmas1 sigmal k rho
    near xv v 1E-6 |@ sprintf "variance identical"
