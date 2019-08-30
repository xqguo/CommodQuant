#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll "
#r "../../packages/Deedle/lib/netstandard2.0/Deedle.dll "
#r "../Library/bin/Debug/netstandard2.0/CommodLib.dll "
#I "../../.paket/load/netstandard2.0/"
#load "FSharp.Data.fsx"
#load "MathNet.Numerics.FSharp.fsx"
#load "QLNet.fsx"
#load "FsCheck.fsx"
do fsi.AddPrinter(fun (printer:Deedle.Internal.IFsiFormattable) -> "\n" + (printer.Format()))
open System
open Deedle 
open FSharp.Reflection
open FsCheck
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open QLNet
open Commod

let (PriceCurve c) = getPrices BRT



