module TestRates

open FsCheck.FSharp
open System
open FsCheck
open FsCheck.Xunit
open FsCheckTypes
open QLNet

//[<Property (Verbose =true) >]
[<Property(EndSize = 1, Arbitrary = [| typeof<ValidDate> |])>]
let ``test discount act365 exp`` (NormalFloat r) (d1: DateTime) (d2: DateTime) =
    //let [d1;d2] = [d1 ; d2] |> List.sort
    let (d1, d2) = if d1 < d2 then (d1, d2) else (d2, d1)
    let days = (d2 - d1).TotalDays

    let rate =
        InterestRate(r, Actual365Fixed(), Compounding.Continuous, Frequency.NoFrequency)

    let disc = rate.discountFactor (Date(d1), Date(d2))
    let disc' = exp (-r * days / 365.)
    near disc disc' (disc * 1E-10)
