namespace Commod
[<AutoOpen>]
module Gabillon = 
    open System
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

    //compute forward covariance between tm to tn for a future with maturity Ti Tj 
    //using constant Gabillon model inputs for this interval
    //allowing different sigmas i vs j.
    let fwdCovariance tm tn Ti Tj sigmasi sigmasj sigmal k rho =
        let Tin = Ti - tn
        let Tim = Ti - tm
        let Tjn = Tj - tn
        let Tjm = Tj - tm
        let TTn = Ti + Tj - 2.0 *tn 
        let TTm = Ti + Tj - 2.0 *tm 
        let ein = exp(-k * Tin)
        let ejn = exp(-k * Tjn)
        let e2n = exp( -k * TTn)
        let eim = exp(-k* Tim)
        let ejm = exp(-k* Tjm)
        let e2m = exp( -k * TTm)
        let einm = ein - eim
        let ejnm = ejn - ejm
        let e2nm = e2n - e2m
        let s2 = sigmasi * sigmasj 
        let l2 = pown sigmal 2
        let p1 = s2 / ( 2.0 * k ) * e2nm 
        let p2 = l2 * ( tn - tm - 1.0/k*(einm + ejnm )+ e2nm /( 2.0 * k))
        let p3 = rho * sigmasi * sigmal * (1.0 / k * einm - e2nm /(2.0 * k) )
        let p4 = rho * sigmasj * sigmal * (1.0 / k * ejnm - e2nm /(2.0 * k) )
        p1 + p2 + p3 + p4

    //compute forward covariance between tm to tn for two futures 1 2 with maturity Ti Tj 
    //using constant Gabillon model inputs for this interval
    //allowing different sigmas i vs j.
    let fwdXCovariance (tm:float) tn Ti Tj sigmas1 sigmas2 sigmal1 sigmal2 k1 k2 rho11 rho12 rho21 rho22 =
        let k12 = k1 + k2
        let Tin = Ti - tn
        let Tim = Ti - tm
        let Tjn = Tj - tn
        let Tjm = Tj - tm
        let ein = exp(-k1 * Tin)
        let ejn = exp(-k2 * Tjn)
        let e2n = exp( -k1 * Tin - k2*Tjn)
        let eim = exp(-k1* Tim)
        let ejm = exp(-k2* Tjm)
        let e2m = exp( -k2 * Tim - k2*Tjm)
        let einmk = (ein - eim)/k1
        let ejnmk = (ejn - ejm)/k2
        let e2nmk = (e2n - e2m)/k12
        let ss = sigmas1 * sigmas2 * rho11
        let ll = sigmal1 * sigmal2 * rho22
        let sl = sigmas1 * sigmal2 * rho12
        let ls = sigmal1 * sigmas2 * rho21
        let p1 = ss * e2nmk 
        let p2 = ll * ( tn - tm - einmk - ejnmk + e2nmk)
        let p3 = sl * ( einmk - e2nmk )
        let p4 = ls * ( ejnmk - e2nmk )
        p1 + p2 + p3 + p4

    let fwdCorr tm tn Ti Tj sigmasi sigmasj sigmal k rho =
        let Vi = fwdVariance tm tn Ti sigmasi sigmal k rho
        let Vj = fwdVariance tm tn Tj sigmasj sigmal k rho
        let cov = fwdCovariance tm tn Ti Tj sigmasi sigmasj sigmal k rho
        cov / sqrt ( Vi * Vj )

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
            
    //get covariance for a gabillon model with vol curve and gabillon global params, and a fixing time vector with corresponding fut contract
    let getGabillonCov ins (vols:VolCurve) (sl, k, rho) (fixings: (DateTime*string) []) pd = 
        let c = getCommod ins
        let optd = c.Contracts.Opt |> Map.map(fun _ d -> getTTM pd d)
        let futd = c.Contracts.Fut |> Map.map(fun _ d -> getTTM pd d)
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

    //get covariance for two gabillon model with vol curve and gabillon global params, and a fixing time vector with corresponding fut contract
    let getXGabillonCov ins (vols:VolCurve)  (fixings: (DateTime*string) []) 
        ins' (vols':VolCurve) (fixings': (DateTime*string) []) 
        (l1, l2, k1, k2, rho1, rho2, rho11, rho12, rho21, rho22 ) pd = 
        let c = getCommod ins
        let optd = c.Contracts.Opt |> Map.map(fun _ d -> getTTM pd d)
        let futd = c.Contracts.Fut |> Map.map(fun _ d -> getTTM pd d)
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
                (c, implySigmaS v v 0. t T l1 k1 rho1 )) 
            |> Map.ofSeq        

        let c' = getCommod ins'
        let optd' = c'.Contracts.Opt |> Map.map(fun _ d -> getTTM pd d)
        let futd' = c'.Contracts.Fut |> Map.map(fun _ d -> getTTM pd d)
        let (ds',cs') = fixings' |> Array.unzip 
        let ts' = ds' |> Array.map (getTTM pd)

        //calibrate ss
        let ss' =
            cs'
            |> set
            |> Set.map( fun c ->            
                let t = optd'.[c]
                let T = futd'.[c]
                let v = vols'.[c] |> float
                (c, implySigmaS v v 0. t T l2 k2 rho2 )) 
            |> Map.ofSeq        

        //compute cov, this one is not symmetric in general
        let n = fixings.Length
        DenseMatrix.init n n ( fun i j -> 
                let t = min ts.[i] ts'.[j]
                let ci = cs.[i]
                let cj = cs'.[j]
                fwdXCovariance 0.0 t futd.[ci] futd'.[cj] ss.[ci] ss'.[cj] l1 l2 k1 k2 rho11 rho12 rho21 rho22 )

    //get covariance for two gabillon model with vol curve and gabillon global params, and a fixing time vector with corresponding fut contract
    let getXGabillonCovFull ins (vols:VolCurve)  (fixings: (DateTime*string) []) 
        ins' (vols':VolCurve) (fixings': (DateTime*string) []) 
        (l1, l2, k1, k2, rho1, rho2, rho11, rho12, rho21, rho22 ) pd = 
        let c = getCommod ins
        let optd = c.Contracts.Opt |> Map.map(fun _ d -> getTTM pd d)
        let futd = c.Contracts.Fut |> Map.map(fun _ d -> getTTM pd d)
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
                (c, implySigmaS v v 0. t T l1 k1 rho1 )) 
            |> Map.ofSeq        

        let c' = getCommod ins'
        let optd' = c'.Contracts.Opt |> Map.map(fun _ d -> getTTM pd d)
        let futd' = c'.Contracts.Fut |> Map.map(fun _ d -> getTTM pd d)
        let (ds',cs') = fixings' |> Array.unzip 
        let ts' = ds' |> Array.map (getTTM pd)

        //calibrate ss
        let ss' =
            cs'
            |> set
            |> Set.map( fun c ->            
                let t = optd'.[c]
                let T = futd'.[c]
                let v = vols'.[c] |> float
                (c, implySigmaS v v 0. t T l2 k2 rho2 )) 
            |> Map.ofSeq        

        //compute cov, this one is not symmetric in general
        let n = fixings.Length
        let m = fixings'.Length
        let sigma12 = DenseMatrix.init n m ( fun i j -> 
                let t = min ts.[i] ts'.[j]
                let ci = cs.[i]
                let cj = cs'.[j]
                fwdXCovariance 0.0 t futd.[ci] futd'.[cj] ss.[ci] ss'.[cj] l1 l2 k1 k2 rho11 rho12 rho21 rho22 )
        
        //compute sigma11 
        let lower = Array2D.init n n ( fun i j -> 
            if i <= j then 
                let t = min ts.[i] ts.[j]
                let ci = cs.[i]
                let cj = cs.[j]
                fwdCovariance 0.0 t futd.[ci] futd.[cj] ss.[ci] ss.[cj] l1 k1 rho1 
            else 0.0 )

        let sigma11 = DenseMatrix.init n n ( fun i j -> if i <= j then lower.[i,j] else lower.[j,i])

        //compute sigma22 
        let lower' = Array2D.init m m ( fun i j -> 
            if i <= j then 
                let t = min ts'.[i] ts'.[j]
                let ci = cs'.[i]
                let cj = cs'.[j]
                fwdCovariance 0.0 t futd'.[ci] futd'.[cj] ss'.[ci] ss'.[cj] l2 k2 rho2 
            else 0.0 )

        let sigma22 = DenseMatrix.init m m ( fun i j -> if i <= j then lower'.[i,j] else lower'.[j,i])

        let cov = sigma11.Append( sigma12 ).Stack((sigma12.Transpose().Append(sigma22)))

        //check for eigenvalue and fix the cov matrix to be posive definite
        let evd = cov.Evd()
        let l = evd.EigenValues |> Vector.map ( fun x -> x.Real)
        let o = evd.EigenVectors
        let l' = l |> Vector.map( fun x -> max x 1E-10 )
        o * (DenseMatrix.ofDiag l') * (o.Transpose())
        
    //hardcoded defaults, could read from files.
    let getGabillonParam ins = 
        match ins with
        | TTF | JKM -> (0.2, 2.5, 0.3)
        | BRT | DBRT -> ( 0.2, 0.8, 0.5)
        | _ -> ( 0.001, 0.001, 0.0 ) //bs equivalent

    let combineGabillonParam (l1,k1,r1) (l2,k2,r2) rho = 
        match rho with 
        | [r] -> 
            (l1,l2,k1,k2,r1,r2, r, r, r, r)
        | [r11;r12;r21;r22] -> 
            (l1,l2,k1,k2,r1,r2, r11, r12, r21, r22)
        | _ -> 
            failwith "Rho should be a list of float of either 1 or 4 elements"

    let getXGabillonParam ins1 ins2 rho = 
        let p1 = getGabillonParam ins1
        let p2 = getGabillonParam ins2
        combineGabillonParam p1 p2 rho
