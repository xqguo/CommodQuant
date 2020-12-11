module Gabillon
open MathNet.Numerics
//compute forward variance between tm to tn for a future with maturity Ti 
//using constant Gabillon model inputs for this interval
let fwdVariance tm tn Ti sigmas sigmal k rho =
    let Tn = Ti - tn
    let Tm = Ti - tm
    let en = exp(-k* Tn)
    let e2n = pown en 2
    let em = exp(-k* Tm)
    let e2m = pown em 2
    let enm = en - em
    let e2nm = e2n - e2m
    let s2 = pown sigmas 2
    let l2 = pown sigmal 2
    let p1 = s2 / ( 2.0 * k ) * e2nm 
    let p2 = l2 * ( tn - tm - 2.0/k*enm + e2nm /( 2.0 * k))
    let p3 = rho * sigmas * sigmal * (2.0/k*enm - e2nm / k )
    p1 + p2 + p3

// derivative of forward variance against sigmas
let dfwdVarianceds tm tn Ti sigmas sigmal k rho =
    let Tn = Ti - tn
    let Tm = Ti - tm
    let en = exp(-k* Tn)
    let e2n = pown en 2
    let em = exp(-k* Tm)
    let e2m = pown em 2
    let enm = en - em
    let e2nm = e2n - e2m
    let s2 = sigmas * 2.0
    let p1 = s2 / ( 2.0 * k ) * e2nm 
    let p3 = rho * sigmal * (2.0/k*enm - e2nm / k )
    p1 + p3

let findSigmaS vm vn tm tn Ti sigmal k rho = 
    let vvt = vm * vm * tm - vn * vn * tn
    RootFinding.RobustNewtonRaphson.FindRoot(
        (fun x ->  vvt - fwdVariance  tm tn Ti x sigmal k rho ),
        (fun x -> dfwdVarianceds  tm tn Ti x sigmal k rho ),
        0.001, vm * 1E3 )
    

   
