namespace Commod
[<AutoOpen>]
module Options =
    open System
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Integration
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.Interpolation
    
    ///returns black scholes fwd price
    let bs f k v t (o:Payoff) = 
        if ( f<=0. || k <= 0. || v<=0. || t<=0. ) then 
            let iv = 
                match o with 
                | Call -> f - k  
                | Put -> k - f  
            (max iv 0.)
        else
            let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
            let d2 = d1 - v*sqrt(t)
            match o with
            | Call -> f*normcdf(d1)-k*normcdf(d2)
            | Put -> k*normcdf(-d2)-f*normcdf(-d1)

    let kirk f1 f2 k v1 v2 rho t o =
        let v = sqrt( v1 * v1 + ( v2 * f2 / ( f2 + k )) ** 2.0 - 2.0 * rho * v1 * v2 * f2 /(f2+k))
        bs f1 (f2+k) v t o

    ///bs fwd delta
    let bsdelta f k v t (o:Payoff) = 
        if ( f<=0. || k <= 0. || v<=0. || t<=0. ) then 
            match o with 
            | Call -> if f >= k  then 1.0 else 0.0
            | Put ->  if f <= k  then 1.0 else 0.
        else
            let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
            //let d2 = d1 - v*sqrt(t)
            match o with
            | Call -> normcdf(d1)
            | Put -> -normcdf(-d1)

    ///bs delta to strike
    let bsstrike f v t delta = 
        let d =  if delta < 0.0 then 1.0 + delta else delta //call delta
        //let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
        let d1 = norminvcdf d 
        f * exp( - d1*v*sqrt(t) + 0.5*v*v*t )

    ///bs quick delta in call
    //computed using atm vol, and center at atmf.
    let bsqdelta f k v t = 
        let d1 = (log(f/k))/(v*sqrt(t))
        normcdf(d1)

    ///bs qdelta to strike
    let bsqstrike f v t delta = 
        //let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
        let d1 = norminvcdf delta 
        f * exp( - d1*v*sqrt(t) )

    //interpolate vol from delta smile using delta
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    let interpolateVolfromDeltaSmile delta (deltas:double[]) (vols:double[]) = 
        let cs = CubicSpline.InterpolateNatural(deltas, vols)
        let d =  if delta < 0.0 then 1.0 + delta else delta //call delta
        cs.Interpolate(d)

    //interpolate vol from delta smile using delta
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    let getVolStrikefromDeltaSmile f t delta (deltas:double[]) (vols:double[]) =        
        let cs = CubicSpline.InterpolateNatural(deltas, vols)
        let d =  if delta < 0.0 then 1.0 + delta else delta //call delta
        let v = cs.Interpolate(d)
        let k = bsstrike f v t d
        [|v;k|]

    //interpolate vol from delta smile
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    let getVolfromDeltaSmile f k t (deltas:double[]) (vols:double[]) =        
        let cs = CubicSpline.InterpolateNatural(deltas, vols)
        RootFinding.Brent.FindRoot(
            (fun x ->  
                let d = bsdelta f k x t Call
                x - cs.Interpolate(d)),
            0.001, 1E3 )

    //interpolate vol from delta smile of
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    //using Gabillon model: option exp/fut exp/long vol/ k / correlation
    let getVolfromDeltaSmileGabillon f x t (deltas:double[]) (vols:double[]) (optT, futT, sl,k,rho)=        
        let cs = CubicSpline.InterpolateNatural(deltas, vols)
        RootFinding.Brent.FindRoot(
            (fun v ->
                let ss = implySigmaS v v 0. optT futT sl k rho  
                let v2t = fwdVariance 0.0 t futT ss sl k rho 
                let v0 = v2t / t |> sqrt 
                let d = bsdelta f x v0 t Call
                v - cs.Interpolate(d)),
            0.001, 1E3 )

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

    let bsGreeks f k v t o  =        
        let h = NumericalDerivative()
        let d = [|f;v;t|]
        let g = 
            ( fun (d:double[]) ->                 
                bs d.[0] k d.[1] d.[2] o )
        [| 
            h.EvaluatePartialDerivative( g, d, 0, 1 ) //delta
            h.EvaluatePartialDerivative( g, d, 0, 2 ) //gamma
            h.EvaluatePartialDerivative( g, d, 1, 1 ) //vega 
            -h.EvaluatePartialDerivative( g, d, 2, 1 ) / 365. //1d theta
        |]
            
