namespace Commod
[<AutoOpen>]
module Gabillon = 
//#r "nuget:MathNet.Numerics"
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Optimization
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

    //(fwdVariance 0.0 1.0 1.0 0.551 0.2 1.0 0.5 )/1.0|> sqrt
    //(fwdVariance 0.0 0.9 1.0 0.551 0.2 1.0 0.5 )/0.9|> sqrt

    let fwdVol tm tn Ti sigmas sigmal k rho =
        let vvt = fwdVariance tm tn Ti sigmas sigmal k rho
        sqrt( vvt / (tn-tm))

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
    
    //get best fit target fun of sum squared error on vol.
    //TODO: fix option exp, using future exp as option exp for now

    let gradientfunc sigmas sigmal k rho (ins:Instrument) pd =
        let c = getCommod ins 
        let vols = getVols ins 
        let targetfunc sigmas sigmal k rho =
            vols.Pillars
            |> Set.filter( fun p -> c.Contracts.[p] > pd )
            |> Set.fold( fun acc p ->
                let t = ( c.Contracts.[p] - pd ).TotalDays / 365.
                let v = vols.[p] |> float
                let v' = fwdVol 0.0 t t sigmas sigmal k rho 
                (pown ( v' - v ) 2 ) + acc ) 0.
        let eps = 1e-6
        let r0 = targetfunc (sigmas+eps) sigmal k rho 
        let r1 = targetfunc sigmas (sigmal+eps) k rho
        let r2 = targetfunc sigmas sigmal (k+eps) rho
        let r3 = targetfunc sigmas sigmal k (rho+eps) 
        let r = targetfunc sigmas sigmal k rho 
        let df x = ( x - r ) / eps
        let d' = [ r0; r1 ; r2; r3 ] |> List.map df |> vector        
        r,d'

    let bestfit ins pd = 
        let solver = Optimization.BfgsBMinimizer( 1e-5, 1e-5, 1e-5, 1000)
        //let solver = Optimization.ConjugateGradientMinimizer(1e-5, 1000)
        let ig = DenseVector.ofList [ 0.5; 0.25; 1.0 ; 0.5 ] //sigmas sigmal k rho
        let u = DenseVector.ofList [ 1.5; 0.5; 10.0 ; 0.9 ] //sigmas sigmal k rho
        let l = DenseVector.ofList [ 0.01; 0.01; 0.01 ; -0.5 ] //sigmas sigmal k rho
        let o = ObjectiveFunction.Gradient( fun (iv:Vector<float>) -> gradientfunc iv.[0] iv.[1] iv.[2] iv.[3] ins pd )        
        solver.FindMinimum( o, l, u, ig)
        //solver.FindMinimum( o, ig)
            
        
