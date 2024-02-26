#r "nuget: FSharp.Data"
#r "nuget: MathNet.Numerics"
#r "nuget: CommodLib, version>=1.0.1"
open System
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open Commod
Commod.IOcsv.ROOT<- __SOURCE_DIRECTORY__ +/ ".." +/ "Library"
reload()

let getGabillonCov' ins (vols:VolCurve) (sl, k, rho) (fixings: (DateTime*string) []) pd = 
    let c = getCommod ins
    let optd = c.Contracts.Opt |> Map.map(fun _ d -> getTTM pd d)

    //for futures vol end date, use season end
    let getSeasonEnd p = 
        let d = pillarToDate p
        dateAdjust' "+3mH+2me" d //Sep (summer end) or March (winter end)
    let futd = c.Contracts.Fut |> Map.map(fun p _ -> getSeasonEnd p |> getTTM pd )
    let (ds,cs) = fixings |> Array.unzip 
    let ts = ds |> Array.map (getTTM pd)

    //calibrate ss
    let ss =
        cs
        |> set
        |> Set.map( fun c ->            
            let t = optd.[c]
            let T = futd.[c]
            let v = vols.[c] |> float
            (c, implySigmaS v v 0. t T sl k rho )) 
        |> Map.ofSeq        

    //compute cov
    let n = fixings.Length
    let lower = Array2D.init n n ( fun i j -> 
        if i <= j then 
            let t = min ts.[i] ts.[j]
            let ci = cs.[i]
            let cj = cs.[j]
            fwdCovariance 0.0 t futd.[ci] futd.[cj] ss.[ci] ss.[cj] sl k rho 
        else 0.0 )

    DenseMatrix.init n n ( fun i j -> if i <= j then lower.[i,j] else lower.[j,i])

let inst1 = TTF
let pricingDate = DateTime.Today
let n = 1.
let d = pricingDate.AddDays(n)
let t = getTTM pricingDate d 
let volcurve1 = getVols inst1 
let fixings1 = Array.init 12 ( fun i -> (d, formatPillar( d.AddMonths (i+1) )))
let sigma1 = getGabillonCov inst1 volcurve1 (getGabillonParam inst1) fixings1 pricingDate 
(sigma1.Diagonal()/t).PointwiseSqrt() |> Vector.toArray

let sigma2 = getGabillonCov' inst1 volcurve1 (getGabillonParam inst1) fixings1 pricingDate 
(sigma2.Diagonal()/t).PointwiseSqrt() |> Vector.toArray //seasonal effect not obvious

let v = sigma1.Diagonal().PointwiseSqrt()
let v2 = v.ToColumnMatrix() * v.ToRowMatrix() 
let corr = sigma1./v2

let v' = sigma2.Diagonal().PointwiseSqrt()
let v2' = v'.ToColumnMatrix() * v'.ToRowMatrix() 
let corr' = sigma2./v2'