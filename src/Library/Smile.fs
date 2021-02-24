namespace Commod
[<AutoOpen>]
module Smile =
    open MathNet.Numerics
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.Interpolation

    //interpolate vol from delta smile using delta
    //deltas should be in range (0, 1) 
    //vols are abs values, e.g 0.2 for 20% vol.
    let interpolateVolfromDeltaSmile delta (deltas:double[]) (vols:double[]) = 
        let cs = CubicSpline.InterpolateNatural(deltas, vols)
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

