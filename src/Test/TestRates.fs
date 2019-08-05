module TestRates
open System
open Xunit
open FsCheck
open FsCheck.Xunit
open FsCheckTypes
open Commod.Utils
open QLNet

//[<Property (Verbose =true) >]
[<Property  >]
let ``test discount act365 exp`` (NormalFloat r ) (d1:DateTime) (d2:DateTime) = 
    ( d1 <= d2 ) ==> lazy(
        let days = (d2 - d1).TotalDays
        let rate = InterestRate(r, Actual365Fixed(), Compounding.Continuous, Frequency.NoFrequency )
        let disc = rate.discountFactor( Date(d1), Date(d2))
        let disc' = exp( -r * days/365.)
        try 
            Assert.Equal( disc', disc, 10 ) 
        with 
        | _ -> Assert.Equal( disc' / disc, 1.0, 10 )
        )
