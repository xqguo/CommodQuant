module TestRates
open System
open FsCheck
open FsCheck.Xunit
open FsCheckTypes
open QLNet

//[<Property (Verbose =true) >]
[<Property( Arbitrary = [| typeof<ValidDate>|])>]
let ``test discount act365 exp`` (NormalFloat r ) (d1:DateTime) (d2:DateTime) = 
    ( d1 <= d2 ) ==> lazy(
        let r = max (min r 1.0 ) -1.0 //limit r in +/- 100% to avoid inf
        let days = (d2 - d1).TotalDays
        let rate = InterestRate(r, Actual365Fixed(), Compounding.Continuous, Frequency.NoFrequency )
        let disc = rate.discountFactor( Date(d1), Date(d2))
        let disc' = exp( -r * days/365.)
        near disc disc' ( disc * 1E-10)
        )
