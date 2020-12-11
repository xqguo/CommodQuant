module Gabillon
//#r "nuget:MathNet.Numerics"
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

//fwdVariance 0.0 1.0 1.0 0.4 0.4 1.0 0.5


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

//dfwdVarianceds 0.0 1.0 1.0 0.4 0.2 1.0 0.5


let findSigmaS vm vn tm tn Ti sigmal k rho = 
    let vvt = vn * vn * tn - vm * vm * tm
    RootFinding.RobustNewtonRaphson.FindRoot(
        (fun x ->  vvt - fwdVariance  tm tn Ti x sigmal k rho ),
        (fun x -> dfwdVarianceds  tm tn Ti x sigmal k rho ),
        0.001, vm * 1E3 )
    
//findSigmaS 0.4 0.4 0.0 1.0 1.0 0.2 1.0 0.5

//can actually analytically compute sigmaS, as it is just a quadratic equation
let implySigmaS vm vn tm tn Ti sigmal k rho = 
    let vvt = vn * vn * tn - vm * vm * tm
    let Tn = Ti - tn
    let Tm = Ti - tm
    let en = exp(-k* Tn)
    let e2n = pown en 2
    let em = exp(-k* Tm)
    let e2m = pown em 2
    let enm = en - em
    let e2nm = e2n - e2m
    let l2 = pown sigmal 2
    let a = 1.0 / ( 2.0 * k ) * e2nm 
    let c = l2 * ( tn - tm - 2.0/k*enm + e2nm /( 2.0 * k)) - vvt
    let b = rho * sigmal * (2.0/k*enm - e2nm / k )
    (-b + sqrt( b * b - 4.0 * a * c ))/(2.0 * a ) //take the bigger root

//implySigmaS 0.4 0.4 0.0 1.0 1.0 0.2 1.0 0.5
   
