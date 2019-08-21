#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll "
#r "../Library/bin/Debug/netstandard2.0/CommodLib.dll"
#I "../../.paket/load/netstandard2.0/"
#load "MathNet.Numerics.FSharp.fsx"
#load "QLNet.fsx"
#load "FsCheck.fsx"
open FsCheck
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open Commod.MC
open System
open QLNet
open Commod.Utils
open Commod
open Commod.DomainTypes
open FSharp.Reflection
open Commod.Charts



