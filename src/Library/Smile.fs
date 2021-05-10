namespace Commod
[<AutoOpen>]
module Smile =
    open MathNet.Numerics
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.Interpolation
    open MathNet.Numerics.Optimization
    open MathNet.Numerics.LinearAlgebra

    let roundp p x = 
        let f = pown 10. p 
        round ( f * x) / f

    //### Fit extreme delta using SVI
    type SVIPara = { a:float ; b:float; s:float; rho:float; m:float; t:float}
    type SVIJwPara = { vt:float ; psit:float; pt:float; ct:float; varmint:float; t:float}

    let svi (psvi:SVIPara) k = 
        psvi.a + psvi.b *(psvi.rho*(k-psvi.m)+ sqrt((k-psvi.m)*(k - psvi.m) + pown psvi.s 2))
    let svivol (psvi:SVIPara) k = sqrt(max 0. (svi psvi (log k))/psvi.t)

    let bestfitsvilogk logk vols t = 
        let gradientfunc a b s rho m k' v' =
            let targetfunc a b s rho m =
                let psvi = {a=a;b=b;s=s;rho=rho;m=m;t=t}
                (v',k')
                ||> Array.map2( fun v k -> 
                    let d = (svi psvi k ) - v
                    pown d 2 )
                |> Array.sum
            let eps = 1e-8
            let r0 = targetfunc (a+eps) b s rho m   
            let r1 = targetfunc a (b+eps) s rho m   
            let r2 = targetfunc a b (s+eps) rho m   
            let r3 = targetfunc a b s (rho+eps) m   
            let r4 = targetfunc a b s rho (m+eps)    
            let r = targetfunc a b s rho m
            let df x = ( x - r ) / eps
            let d' = [ r0; r1 ; r2; r3 ;r4 ] |> List.map df |> vector        
            r,d'     
        let v' = vols |> Array.map( fun x -> (pown x 2) * t )
        let solver = BfgsBMinimizer( 1e-12, 1e-12, 1e-12, 1000000)
        let ig = DenseVector.ofList [ 0.1; 0.1; 0.1; 0. ; 0. ]
        let l = DenseVector.ofList [ -10.;1e-5; 1e-5; -0.999 ; -10. ] 
        let u = DenseVector.ofList [ 10.;100.0; 100.; 0.999 ; 10. ] 
        let o = ObjectiveFunction.Gradient( fun (iv:Vector<float>) -> gradientfunc iv.[0] iv.[1] iv.[2] iv.[3] iv.[4] logk v')
        solver.FindMinimum( o, l, u, ig)
            
    let sviToJw (psvi:SVIPara) = 
        let a = psvi.a
        let b = psvi.b
        let s = psvi.s
        let m = psvi.m
        let r = psvi.rho
        let t = psvi.t
        let vt = (a+b*(-r*m+sqrt( pown m 2 + pown s 2)))/t 
        let bhat = sqrt(1.0/(vt*t))*b
        let psit = bhat/2.0*(-m/sqrt(pown m 2+ pown s 2)+r)
        let pt = bhat*(1.0-r)
        let ct = bhat*(1.0+r)
        let varmint = (a+b*abs(s)*sqrt(1.0-pown r 2))/t
        {vt=( sqrt vt );psit=psit;pt=pt;ct=ct;varmint=(sqrt varmint);t=t}

    let bestfitsvi deltas vols t = 
        let strikes = (deltas,vols) ||> Array.map2( fun d v -> bsstrike 1.0 v t d |> log )
        bestfitsvilogk strikes vols t  

    let jwToSvi (pjw:SVIJwPara) =
        let vt = pown pjw.vt 2
        let pt = pjw.pt
        let ct = pjw.ct
        let varmint = pown pjw.varmint 2
        let texp = pjw.t
        let psit = pjw.psit
        let sqrtw = sqrt(vt*texp)
        let bhat = (pt+ct)/2.
        let b =bhat*sqrtw
        let rho = min (max ( 1.0 - pt/bhat) -0.999) 0.999
        let bet = rho - 2.0*psit/bhat
        let bet2 = min (max ( bet * bet ) 0.001) 0.999
        let alpha = (float (sign bet) )*sqrt(1.0/bet2 - 1.0);
        let m = (vt-varmint)*texp/b/(-rho+ (float (sign alpha))*sqrt(1.0+ pown alpha 2)-alpha*sqrt(1.0 - pown rho 2))
        let s = alpha*m
        let a = varmint*texp - b*s*sqrt(1.0 - pown rho 2)
        {a=a;b=b;s=s;rho=rho;m=m;t=texp}

    let bsvol f k t (o:Payoff) p  =        
        RootFinding.Brent.FindRoot(
            (fun v -> 
                let p' = bs f k v t o
                p - p'),
            0.01, 2.0 )

    let interpolateSVIVolfromDelta (psvi:SVIPara) d = 
        let t = psvi.t    
        let k' = 
            RootFinding.Brent.FindRoot(
                (fun k -> 
                    let v = svivol psvi k  
                    let d1 = bsdelta 1.0 k v t Call
                    d1 - d),
                0.0001, 10.0 )
        svivol psvi k' 

    let interpolateVolCSlogkfromDeltaSmile k t (deltas:double[]) (vols:double[]) = 
        let logk, v2 = 
            (deltas,vols)
            ||> Array.map2( fun d v -> 
                let k = bsstrike 1.0 v t d
                log k, v*v)
            |> Array.sortBy fst
            |> Array.unzip
        let cs = CubicSpline.InterpolatePchipSorted(logk, v2)
        // let cs = CubicSpline.InterpolateAkimaSorted (logk, v2)
        cs.Interpolate( log k) 
        |> sqrt

    let interpolateVolbyDeltaCSlogk d t (deltas:double[]) (vols:double[]) = 
        let getv k = interpolateVolCSlogkfromDeltaSmile k t deltas vols 
        let n = deltas.Length    
        let strikes = (deltas,vols) ||> Array.map2( fun d v -> bsstrike 1.0 v t d)
        let (l,h) = 
            if d <= deltas.[0] then 
                ( strikes.[0] - 1E-4, strikes.[0] + 1. )
            elif d >= deltas.[n-1] then
                ( strikes.[n-1] / 2.0 , strikes.[n-1] + 1E-4)
            else  
                let i = deltas |> Array.findIndex( fun x -> x > d )
                (strikes.[i] - 1E-4, strikes.[i-1] + 1E-4)
        let k = 
            try 
                RootFinding.Brent.FindRoot(
                    (fun k -> 
                        // let k = bsstrike 1.0 v t d
                        let v = getv k
                        let d' = bsdelta 1.0 k v t Call
                        // printfn "v=%f;k=%f;d=%f;d'=%f" v k d d'
                        d' - d ),
                    l, h )
            with
            | e -> failwithf "d:%f, t:%f,\ndeltas:%A,\nvols:%A,\nerror:%A" d t deltas vols e
        getv k 

    //interpolate vol from delta smile using delta
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    let interpolateVolfromDeltaSmile delta (deltas:double[]) (vols:double[]) = 
        let cs = CubicSpline.InterpolatePchipSorted(deltas, vols)
        let d =  if delta < 0.0 then 1.0 + delta else delta //call delta
        cs.Interpolate(d)

    let getVolCurveFromSmile d (smile:VolDeltaSmile) =
        let p = smile.Pillars
        let deltas = smile.Deltas
        let vols = smile.Vols
        let vs = 
            match Array.tryFindIndex( fun x -> x = d ) deltas with 
            | Some i -> 
                vols |> Array.map( fun v -> v.[i] |> decimal |> AbsoluteVol )
            | None -> 
                vols |> Array.map( fun v -> interpolateVolfromDeltaSmile d deltas v |> decimal |> AbsoluteVol )
        Array.zip p vs |> Map.ofArray |> VolCurve

    //interpolate vol from delta smile using delta
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    let getVolStrikefromDeltaSmile f t delta (deltas:double[]) (vols:double[]) =        
        let d =  if delta < 0.0 then 1.0 + delta else delta //call delta
        let v = interpolateVolfromDeltaSmile d deltas vols
        let k = bsstrike f v t d
        [|v;k|]

    //compute delta for strike from delta smile of
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    let getDeltafromDeltaSmile f k t (deltas:double[]) (vols:double[]) =        
        let g d = interpolateVolfromDeltaSmile d deltas vols
        RootFinding.Brent.FindRoot(
            (fun d -> 
                let v = g d
                let d' = bsdelta f k v t Call
                d - d'),
            0.0, 1.0 )

    //interpolate vol from delta smile
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    let getVolfromDeltaSmile f k t (deltas:double[]) (vols:double[]) =        
        let d = getDeltafromDeltaSmile f k t deltas vols 
        interpolateVolfromDeltaSmile d deltas vols

    //compute delta for strike from delta smile of
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    //using Gabillon model: option exp/fut exp/long vol/ k / correlation
    let getDeltafromDeltaSmileGabillon f x t (deltas:double[]) (vols:double[]) (optT, futT, sl,k,rho)=        
        let g d = interpolateVolfromDeltaSmile d deltas vols
        RootFinding.Brent.FindRoot( (fun d ->
            let v = g d 
            let ss = implySigmaS v v 0. optT futT sl k rho  
            let v2t = fwdVariance 0.0 t futT ss sl k rho 
            let v0 = v2t / t |> sqrt 
            let d' = bsdelta f x v0 t Call
            d - d'),
            0.0, 1.00 )

    let bsDeltaSmile f k t o (deltas:double[]) (vols:double[]) =        
        let v = getVolfromDeltaSmile f k t deltas vols 
        bs f k v t o 

    let bsDeltaSmileWithDelta f d t (deltas:double[]) (vols:double[]) =        
        let r = getVolStrikefromDeltaSmile f t d deltas vols 
        let o = if d >= 0.0 then Call else Put
        (bs f r.[1] r.[0] t o , r.[0], r.[1]) // prem, v, strike.

    let bsAdaptedGreeks f k t o (deltas:double[]) (vols:double[]) =        
        let h = NumericalDerivative()
        let d = [|f;0.0;t|]
        let g = 
            ( fun (d:double[]) ->                 
                let vols' = vols |> Array.map( fun x -> x + d.[1])
                bsDeltaSmile d.[0] k d.[2] o deltas vols' )
        [| 
            h.EvaluatePartialDerivative( g, d, 0, 1 ) //delta
            h.EvaluatePartialDerivative( g, d, 0, 2 ) //gamma
            h.EvaluatePartialDerivative( g, d, 1, 1 ) //vega 
            -h.EvaluatePartialDerivative( g, d, 2, 1 ) / 365. //1d theta
        |]

    ///use reference ETO's delta to get VolCurve from smile, flat vol
    let getRefDelta f k expDate refMonth (smile:VolDeltaSmile) pd = 
        let t = getTTM pd expDate
        let deltas = smile.Deltas
        let n = smile.Pillars |> Array.findIndex ((=) refMonth)
        let vols = smile.Vols.[n]
        let v' = getVolfromDeltaSmile f k t deltas vols
        let d' = bsdelta f k v' t Call 
        getVolCurveFromSmile d' smile

    ///use reference ETO's delta to get VolCurve from smile using Gabilion model
    let getRefDeltaGabillon f k expDate refMonth (smile:VolDeltaSmile) ot ft (lv,b,r) pd = 
        let t = getTTM pd expDate
        let eo = getTTM pd ot
        let ef = getTTM pd ft
        let deltas = smile.Deltas
        let n = smile.Pillars |> Array.findIndex ((=) refMonth)
        let vols = smile.Vols.[n]
        let d = getDeltafromDeltaSmileGabillon f k t deltas vols (eo,ef,lv,b,r)
        getVolCurveFromSmile d smile
