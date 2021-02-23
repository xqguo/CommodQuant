namespace Commod
[<AutoOpen>]
module Options =
    open System
    open MathNet.Numerics
    open MathNet.Numerics.Distributions
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Integration
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.Interpolation
    
    type Payoff = 
        | Call 
        | Put 

    let normcdf = fun x -> Normal.CDF ( 0., 1., x )  
    let normpdf = fun x -> Normal.PDF ( 0., 1., x )  
    let norminvcdf = fun x -> Normal.InvCDF( 0., 1., x )

    /// helper function to get multi-dim GH zs 
    /// permulate x y into x*y entries of zs.
    let private permutate x y = 
        let xi = Array.length x //x is the new head
        let yi = Array.length y //y is the exisiting tail
        [| 
            for i in 0 .. (xi - 1 ) do 
            for j in 0 .. (yi - 1 ) do 
            yield Array.append [|x.[i]|] y.[j] 
        |]

    ///helper function to get multi-dim GH weigths.
    ///cumulative prod of normalized weights
    let private permprod x y = 
        let xi = Array.length x //x is the new head
        let yi = Array.length y //y is the exisiting tail
        [| 
            for i in 0 .. (xi - 1 ) do 
            for j in 0 .. (yi - 1 ) do 
            yield x.[i] * y.[j] 
        |]

    //recursive version with dim input
    //https://en.wikipedia.org/wiki/Gauss%E2%80%93Hermite_quadrature
    // 1 <= dim 
    let rec ghzn (o:int list) =     
        let s2 = sqrt 2.0
        let z n= 
            //https://keisan.casio.com/exec/system/1281195844
            match n with 
            | 2 -> [|-0.7071067811865475244008;0.7071067811865475244008|]
            | 3 -> [|-1.2247448714;  0. ;1.2247448714|] 
            | 5 -> [|
                    -2.020182870456085632929
                    -0.9585724646138185071128
                    0.
                    0.9585724646138185071128
                    2.020182870456085632929 |]
            | 7 -> [|
                    -2.651961356835233492447
                    -1.673551628767471445032
                    -0.8162878828589646630387
                    0.
                    0.8162878828589646630387
                    1.673551628767471445032
                    2.651961356835233492447 |]
            | 17 -> [|
                    -4.871345193674403088349
                    -4.061946675875474306892
                    -3.378932091141494083383
                    -2.757762915703888730926
                    -2.173502826666620819275
                    -1.612924314221231333113
                    -1.067648725743450553631
                    -0.5316330013426547313491
                    0.
                    0.5316330013426547313491
                    1.067648725743450553631
                    1.612924314221231333113
                    2.173502826666620819275
                    2.757762915703888730926
                    3.378932091141494083383
                    4.061946675875474306892
                    4.871345193674403088349|]
                | 42 -> [|
                    -8.325809389566931216288
                    -7.644783295704741859325
                    -7.078867873049108721921
                    -6.57201717138747510262
                    -6.10285233438152662139
                    -5.660357581283057698747
                    -5.237915885017649930593
                    -4.831153629128275966268
                    -4.4369817058810309607
                    -4.053107744424767000616
                    -3.677763316388556876407
                    -3.30954009651092198804
                    -2.947285782305479233738
                    -2.590034870617126460575
                    -2.23696078705431805169
                    -1.887341620543484820361
                    -1.540534800915545895967
                    -1.195957794377809835979
                    -0.8530729091605537180465
                    -0.5113749183154693494509
                    -0.170380585561816973074
                    0.170380585561816973074
                    0.511374918315469349451
                    0.8530729091605537180465
                    1.195957794377809835979
                    1.540534800915545895967
                    1.887341620543484820361
                    2.23696078705431805169
                    2.590034870617126460575
                    2.947285782305479233738
                    3.30954009651092198804
                    3.677763316388556876407
                    4.053107744424767000616
                    4.4369817058810309607
                    4.831153629128275966268
                    5.237915885017649930593
                    5.660357581283057698747
                    6.10285233438152662139
                    6.57201717138747510262
                    7.078867873049108721921
                    7.644783295704741859325
                    8.325809389566931216288            |]
            | _ -> failwith "Not implemented"
            |> Array.map ( fun x -> x * s2)
        match o with
        | [] -> invalidArg "o" "order should not be empty list"
        | [h] ->     
            [| for x in (z h) do yield [|x|]|]
        | h::t -> 
            permutate (z h) (ghzn t) //previous layer)

    let rec ghwn (o:int list) =     
        let cons = sqrt(System.Math.PI)
        let w n = 
            //https://keisan.casio.com/exec/system/1281195844
            match n with 
            | 2 -> [|0.8862269254527580136491; 0.8862269254527580136491|]
            | 3 -> [|0.2954089752; 1.1816359006 ; 0.2954089752|] 
            | 5 -> [|
                    0.01995324205904591320774
                    0.3936193231522411598285
                    0.9453087204829418812257
                    0.393619323152241159828
                    0.01995324205904591320774 |] 
            | 7 -> [|
                    9.71781245099519154149E-4
                    0.05451558281912703059218
                    0.4256072526101278005203
                    0.810264617556807326765
                    0.4256072526101278005203
                    0.0545155828191270305922
                    9.71781245099519154149E-4 |]
            | 17 -> [|
                    4.58057893079863330581E-11
                    4.97707898163079405228E-8
                    7.11228914002130958353E-6
                    2.986432866977530411513E-4
                    0.0050673499576275379117
                    0.0409200341497562798095
                    0.1726482976700970792177
                    0.4018264694704119565776
                    0.5309179376248635603319
                    0.4018264694704119565776
                    0.1726482976700970792177
                    0.0409200341497562798095
                    0.005067349957627537911701
                    2.98643286697753041151E-4
                    7.11228914002130958353E-6
                    4.97707898163079405228E-8
                    4.58057893079863330581E-11                    
                    |]
                | 42 -> [|
                    6.1678589258107142561E-31
                    2.5278698640535659933E-26
                    9.1778906956924076508E-23
                    8.4821520800862769186E-20
                    3.03589034781071776352E-17
                    5.2533377155685611014E-15
                    5.03270558218403059708E-13
                    2.92172883723335694663E-11
                    1.09580522880784158623E-9
                    2.7834715265490756444E-8
                    4.96365939357983527576E-7
                    6.3902459677354268946E-6
                    6.0719621077883351208E-5
                    4.334122717212549994714E-4
                    0.00235716139459631447181
                    0.00987952405318850910393
                    0.03220210128890782981979
                    0.0822112693032937689871
                    0.1652880012746674607125
                    0.262738906782294764379
                    0.331048913890856797403
                    0.3310489138908567974027
                    0.2627389067822947643795
                    0.165288001274667460713
                    0.0822112693032937689871
                    0.03220210128890782981979
                    0.00987952405318850910393
                    0.00235716139459631447181
                    4.33412271721254999471E-4
                    6.07196210778833512076E-5
                    6.39024596773542689462E-6
                    4.9636593935798352758E-7
                    2.78347152654907564443E-8
                    1.09580522880784158623E-9
                    2.92172883723335694663E-11
                    5.03270558218403059708E-13
                    5.2533377155685611014E-15
                    3.03589034781071776352E-17
                    8.4821520800862769186E-20
                    9.1778906956924076508E-23
                    2.52786986405356599327E-26
                    6.16785892581071425612E-31|]
            | _ -> failwith "Not implemented"
            |> Array.map ( fun x -> x / cons )
        match o with
        | [] -> invalidArg "o" "order should not be empty"
        | [h] -> w h
        | h::t -> permprod (w h) (ghwn t)

    let ghz5 d = ghzn (List.replicate d 5)
    let ghw5 d = ghwn  (List.replicate d 5)     
    let ghz3 d = ghzn (List.replicate d 3)
    let ghw3 d = ghwn (List.replicate d 3)      
    let gh o = ghzn o , ghwn o 

    // o is a list of order of Ghass Hermite for each dim of integration
    let ghint (o:int list) (f:(float[]->float)) =
        let zs,ws = gh o 
        let o = zs |> Array.map f
        (o,ws) ||> Array.map2 (*) |> Array.sum

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
        let f = f1 / ( f2 + k )
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
            
    // a time matrix for VCV using min ( T1 T2 )
    let private getTmatrix (T1:Vector<float>) (T2:Vector<float>) =
        let onesT1 = Vector.Build.Dense (T1.Count, 1.)
        let onesT2 = Vector.Build.Dense( T2.Count , 1. )
        let tmatrix2 = onesT1.OuterProduct T2
        let tmatrix1 = T1.OuterProduct onesT2
        tmatrix1.PointwiseMinimum tmatrix2

    let private sum (m:Matrix<float>) = 
        m.RowSums().Sum()

    ///2 moment matching
    let moments (f:Vector<float>) (V1:Vector<float>) (T1:Vector<float>) = 
        let x1 = f.Sum()
        let tmatrix = getTmatrix T1 T1
        let vv = ((V1.OuterProduct  V1) .* tmatrix ).PointwiseExp() //assuming constant vol per forward, used for com, more generally could sum piece-wise 
        let x11 = ((f.OuterProduct f) .* vv ) |> sum
        (x1, x11, 0.)

    ///cross moments matrix
    let momentsx (f1w:Vector<float>) (v1:Vector<float>) (t1:Vector<float>) (f2w:Vector<float>) (v2:Vector<float>) (t2:Vector<float>) rho = 
        let ff = f1w.OuterProduct f2w
        if (rho = 0.) then 
            ff //all cross terms are 0.
        else
            let tmatrix = getTmatrix t1 t2
            (ff .* ((v1.OuterProduct v2).*tmatrix*rho).PointwiseExp()) 
        |> sum

    ///3rd cross moment from fwd and 2nd moment matrix ( which is f1f2exp(var))
    let momentsx3 (f:Vector<float>) (v:Matrix<float>) = 
        let n = f.Count - 1 
        let m1 = f.Sum()
        let m2 = v |> sum
        let m3 = 
            [|
                for i in 0 .. n do
                for j in 0 .. n do
                for k in 0 .. n do 
                    let f3 = f.[i] * f.[j] * f.[k]
                    let v3 = v.[i,j] * v.[j,k] * v.[k,i]
                    yield v3 / f3
            |]
            |> Array.sum
        let u1 = m1
        let u2 = m2 - u1 * u1
        let u3 = m3 - 3.0*u1*u2 - u1*u1*u1
        let z = ((u3 + sqrt(u3 * u3 + 4.0 * (pown u2 3 )))/2.0)**(1.0/3.0)
        let y1 = u2 / ( z - u2/z )
        let y11 = u2 + y1 * y1
        let x = u1 - y1
        (y1,y11,x)

    //asian fwd price moment matching method
    let asianoption (f1:Vector<float>) (fw1:Vector<float>) t1 v1 k' o p1w =
        let (y1, y11, delta) = moments (f1 .* fw1) v1 t1  
        let k = k' - p1w - delta
        let v = sqrt( log (y11/y1/y1) )
        bs y1 k v 1. o 

    //asian fwd price moment matching method
    let asianOptionAndDelta (f1:Vector<float>) (fw1:Vector<float>) t1 v1 k' o p1w =
        let (y1, y11, delta) = moments (f1 .* fw1) v1 t1  
        let k = k' - p1w - delta
        let v = sqrt( log (y11/y1/y1) )
        bs y1 k v 1. o ,
        (bsdelta y1 k v 1. o ) * fw1.Sum()

    ///spread option fwd pricing moment matching
    let rec spreadoption (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) v2 k (rho:float) callput 
        (p1:Vector<float>) (pw1:Vector<float>) (p2:Vector<float>) (pw2:Vector<float>)= 
            let f1w = f1 .* fw1 //1st asset weighted
            let f2w = f2 .* fw2
            //#1st moments
            let x1 = f1w.Sum()
            let x2 = f2w.Sum()
            let k' = k - (p1 .* pw1).Sum() + (p2 .* pw2 ).Sum() // adapte K for past fixings
            if k' < 0. then
                let v0 = Vector<float>.Build.Dense(1) 
                let callput' = match callput with | Call -> Put | Put -> Call
                spreadoption f2 fw2 t2 v2 f1 fw1 t1 v1 -k' rho callput' v0 v0 v0 v0 //put equivalent
            else 
            //#2nd moments
                let tmatrix1 = getTmatrix t1 t1
                let x11 = ((f1w.OuterProduct f1w).*exp( (v1.OuterProduct v1) .* tmatrix1)) |> sum
                let tmatrix2 = getTmatrix t2 t2
                let x22 = ((f2w.OuterProduct f2w).*exp( (v2.OuterProduct v2) .* tmatrix2)) |> sum
                let tmatrix = getTmatrix t1 t2
                //let x12 = ((f1w.OuterProduct f2w).*exp( (v1.OuterProduct v2) .* tmatrix * rho )) |> sum
                let x12 = momentsx f1w v1 t1 f2w v2 t2 rho
            //#intermediates
                let b1 = sqrt(log(x22 / x2 / x2 ))
                let b2 = 1. / b1 * log(x12 / (x1 * x2))
                let cp = match callput with |Call -> 1.0 |Put -> -1.0
                let g = cp / sqrt(max (log(x11 / x1 / x1) - b2 * b2) 1E-12) //this can be nan? 
                let i1 = Func<float, float> (fun x -> normpdf(x) * normcdf(g * log(sqrt(x11) * exp(b2 * x) / (k + x2 * x12 / (x1 * sqrt(x22)) * exp(b1 * x)))))
                let i2 = Func<float, float>(fun x -> normpdf(x) * normcdf(g * log(x1 * x12 / (x2 * sqrt(x11)) * exp(b2 * x) / (k + sqrt(x22) * exp(b1 * x)))))
                let i3 = Func<float,float>( fun x -> normpdf(x) * normcdf(g * log(x1 * x1 / sqrt(x11) * exp(b2 * x) / (k + x2 * x2 / sqrt(x22) * exp(b1 * x)))))
                let stddevs = 6.0
                let partitions = 100000
                let i1' = SimpsonRule.IntegrateComposite( i1, -stddevs, stddevs, partitions)
                let i2' = SimpsonRule.IntegrateComposite( i2, -stddevs, stddevs, partitions)
                let i3' = SimpsonRule.IntegrateComposite( i3, -stddevs, stddevs, partitions)
                cp * ( x1 * i1' - x2 * i2' - k * i3')

    /// append 2 vectors
    let appendVector (v1:Vector<float>) (v2:Vector<float>) =
        v1.ToColumnMatrix().Stack(v2.ToColumnMatrix()).Column(0);

    ///householder reflection matrix
    let householderR (q:Vector<float>) =
        let n = q.Count
        let e = DenseVector.init n (fun n -> if n = 0 then 1.0 else 0.0)
        let q' = (q - e )
        let v = q' / q'.L2Norm()
        (DiagonalMatrix.identity n ) - 2.0 * v.OuterProduct(v)

    ///covariance Sigma2 = rho v_k v_j min(t_k, t_j)
    let getSigma2 (v1:Vector<float>) (t1:Vector<float>) (v2:Vector<float>) (t2:Vector<float>) rho =                
        let tmatrix = getTmatrix t1 t2
        (v1.OuterProduct v2).*tmatrix*rho


    ///get cov for 2 assets with constant correlation.
    let getCov (t1:Vector<float>) (v1:Vector<float>) (t2:Vector<float>) (v2:Vector<float>) (rho:float) = 
            //assuming constant correlation between 2 assets.
            let sigma12 = getSigma2 v1 t1 v2 t2 rho
            let sigma11 = getSigma2 v1 t1 v1 t1 1.0
            let sigma22 = getSigma2 v2 t2 v2 t2 1.0
            sigma11.Append( sigma12 ).Stack((sigma12.Transpose().Append(sigma22)))

    ///get cov for 2 assets with constant correlation with cov inputs.
    let getCov2 (t1:Vector<float>) (sigma11:Matrix<float>) (t2:Vector<float>) (sigma22:Matrix<float>) (rho:float) = 
            //assuming constant correlation between 2 assets.
            let v1 = (sigma11.Diagonal() ./ t1) |> Vector.Sqrt
            let v2 = (sigma22.Diagonal() ./ t2) |> Vector.Sqrt
            let sigma12 = getSigma2 v1 t1 v2 t2 rho
            sigma11.Append( sigma12 ).Stack((sigma12.Transpose().Append(sigma22)))

    ///get V with Choi's method with forward weights and cov, general case
    let getVChoi (f:Vector<float>) (w:Vector<float>) (sigma:Matrix<float>) = 
            let v = sigma.Diagonal().PointwiseSqrt()
            let n = f.Count
            let g' = f.*w
            let g = (g'/ (g'.L2Norm()))
            let c = sigma.Cholesky().Factor //cholesky
            let den = sqrt(( g.ToRowMatrix() * sigma * g.ToColumnMatrix()).Item(0,0)) 
            let Q1 = (c.Transpose() * g.ToColumnMatrix() ) /den
            let V1 = (c * Q1)// need to adjust so that w_k V_k1 > 0
            //let eps = 0.01 //choose this following Choi          
            let eps = 1E-10 //increase eps to check if high corr case improves          
            let V1' = DenseVector.init n ( fun n -> 
                if V1.[n,0] * w.[n] > 0. 
                then V1.[n,0]
                else
                   let s = if w.[n] > 0. then 1.0 else -1.0
                   eps * s )       
                   //eps * s * v.[n] )       
            let mu = (c.Inverse() * V1').L2Norm()
            let V1'' = V1' / mu
            let Q1' = c.Inverse() * V1''
            let R = householderR Q1'
            let CR = c * R 
            let CR' = CR.SubMatrix(0, n, 1, n-1) //drop 1st column.
            let svd = CR'.Svd()      
            let V = V1''.ToColumnMatrix().Append(svd.U * svd.W)
            V

    ///get V with Choi's method for 2 assets with constant correlation.
    let getVChoi2Asset (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) (v2:Vector<float>) (rho:float) = 
            let f = appendVector f1 f2
            let w = appendVector fw1 (fw2 * -1.)
            let sigma = getCov t1 v1 t2 v2 rho
            getVChoi f w sigma

    /////get V with Choi's method for 2 assets with constant correlation, deprecated. 
    //let getVChoi'' (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
    //    (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) (v2:Vector<float>) (rho:float) = 
    //        let f1w = f1 .* fw1 //long side weighted
    //        let f2w = f2 .* fw2 * -1. //short side weighted
    //        let w = appendVector fw1 (fw2 * -1.)
    //        let v = appendVector v1 v2 
    //        let n = f1.Count + f2.Count
    //        //assuming constant correlation between 2 assets.
    //        let g' = appendVector f1w f2w  
    //        let g = (g'/ (g'.L2Norm()))
    //        let sigma12 = getSigma2 v1 t1 v2 t2 rho
    //        let sigma11 = getSigma2 v1 t1 v1 t1 1.0
    //        let sigma22 = getSigma2 v2 t2 v2 t2 1.0
    //        //printfn "sigma12 dims: %i %i" sigma12.RowCount sigma12.ColumnCount
    //        //printfn "sigma11 dims: %i %i" sigma11.RowCount sigma11.ColumnCount
    //        //printfn "sigma22 dims: %i %i" sigma22.RowCount sigma22.ColumnCount
    //        let sigma = sigma11.Append( sigma12 ).Stack((sigma12.Transpose().Append(sigma22)))
    //        //printfn "%A" sigma
    //        let c = sigma.Cholesky().Factor //cholesky
    //        //let den2 = g.ToRowMatrix() * sigma * g.ToColumnMatrix() //should be a scalar 
    //        let den = sqrt(( g.ToRowMatrix() * sigma * g.ToColumnMatrix()).Item(0,0)) //should be a scalar 
    //        let Q1 = (c.Transpose() * g.ToColumnMatrix() ) /den
    //        let V1 = (c * Q1)// need to adjust so that w_k V_k1 > 0
    //        //printfn "v1: %A" (V1 |> Matrix.toArray2)
    //        let eps = 0.01 //choose this following Choi          
    //        let V1' = DenseVector.init n ( fun n -> 
    //            if V1.[n,0] * w.[n] > 0. 
    //            then V1.[n,0]
    //            else
    //               let s = if w.[n] > 0. then 1.0 else -1.0
    //               eps * s * v.[n] )       
    //        //let a = V1.L2Norm()
    //        //let b = V1'.L2Norm()
    //        let mu = (c.Inverse() * V1').L2Norm()
    //        //let mu = V1.L2Norm() / V1'.L2Norm()
    //        let V1'' = V1' / mu
    //        //let mu' = (c.Inverse() * V1'').L2Norm()
    //        //let mu = 0.99 //how to choose this? need to maintain norm same as vol1
    //        //V1 |> Matrix.toArray2
    //        //V1'.L2Norm()
    //        let Q1' = c.Inverse() * V1''
    //        let R = householderR Q1'
    //        let CR = c * R 
    //        let CR' = CR.SubMatrix(0, n, 1, n-1) //drop 1st column.
    //        let svd = CR'.Svd()      
    //        //let qdot = svd.VT
    //        let V = V1''.ToColumnMatrix().Append(     svd.U * svd.W)
    //        //let Q'= DenseMatrix.init n n ( fun i j -> 
    //        //    match i,j with
    //        //    | 0, 0 -> 1.0
    //        //    | 0, _ -> 0.0
    //        //    | _, 0 -> 0.0
    //        //    | _ , _ -> qdot.[i-1,j-1]
    //        //    ) 
    //        //let V = c* R* Q' //V
    //        //let test = (V * V.Transpose()) //is input covariance matrix
    //        V

    let consolidateInputs (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) =
        //consolidate future details to group same fixing dates
        //but retain the same weights, so that the delta still works
        let (f, t, v ) = 
            Array.zip3  ((f1 .* fw1).ToArray()) (t1.ToArray())  (v1.ToArray())              
            |> Array.groupBy(fun (_,t,_) -> t) 
            |> Array.map( fun (t0,r) -> 
                let f,t,v = Array.unzip3 r
                let (y1, y11, _) = moments (vector f)  (vector v) (vector t  )
                y1, t0, sqrt( log(y11/y1/y1) / t0 ))     
            |> Array.unzip3
        let fv = vector f
        let tv = vector t
        let vv = vector v
        //let w = DenseVector.create fv.Count 1.
        let w = 
            Array.zip  (fw1.ToArray()) (t1.ToArray())              
            |> Array.groupBy( snd ) 
            |> Array.map(fun (_, r ) -> r |> Array.sumBy fst )
            |> vector
        let fv' = fv / w //TODO need to handle case where w is 0??? e.g. two forward fixing opposite weight, different value?
        fv', w, tv, vv

    /////Choi method of analytical 1 layer + GH remianing.
    /////assuming perferect correalation in asset
    //let rec optionChoi2Asset (f1':Vector<float>) (fw1':Vector<float>) (t1':Vector<float>) (v1':Vector<float>) 
    //    (f2':Vector<float>) (fw2':Vector<float>) (t2':Vector<float>) v2' k' (rho:float) callput 
    //    (p1:Vector<float>) (pw1:Vector<float>) (p2:Vector<float>) (pw2:Vector<float>) = 
    //    //validate inputs
    //    if Vector.exists (fun x -> x <= 0. ) t1' then invalidArg "t1'" "time to matuirty needs to be positive values"
    //    if Vector.exists (fun x -> x <= 0. ) t2' then invalidArg "t1'" "time to matuirty needs to be positive values"
    //    if Vector.exists (fun x -> x <= 0. ) v1' then invalidArg "v1'" "vol needs to be positive values"
    //    if Vector.exists (fun x -> x <= 0. ) v2' then invalidArg "v2'" "vol needs to be positive values"
    //    let f1,fw1,t1,v1 = consolidateInputs f1' fw1' t1' v1'
    //    let f2,fw2,t2,v2 = consolidateInputs f2' fw2' t2' v2' 
    //    let strike = k' - (p1 .* pw1).Sum() + (p2 .* pw2 ).Sum() // adapte K for past fixings
    //    if strike < 0. then 
    //        let v0 = Vector<float>.Build.Dense(1) 
    //        let callput' = match callput with | Call -> Put | Put -> Call
    //        let (opt,delta) = optionChoi2Asset f2 fw2 t2 v2 f1 fw1 t1 v1 -strike rho callput' v0 v0 v0 v0 //put equivalent
    //        opt, (delta |> Array.rev) //delta sequence reverted.
    //    else
    //        let weights = appendVector fw1 (-1. * fw2)
    //        let V = getVChoi f1 fw1 t1 v1 f2 fw2  t2 v2 rho
    //        //for each z_dot, find z1 and then C_bs, and then sum them using GH
    //        let n = f1.Count + f2.Count
    //        let fk k (z:Vector<float>) = //formula 7 
    //            let vk = V.Row(k) //kth row vector
    //            let vksum = vk * vk  - vk.[0] * vk.[0]
    //            exp( -0.5 * vksum + vk.SubVector(1,z.Count) * z ) //fk for some z

    //        let F = appendVector f1 f2 
    //        let dim = min (n-1) 4 //cap at 5 levels
    //        if dim = 0 then failwith "degenerated bs formula directly, not implemented yet"
    //        let zs = ghz5 dim 
    //        let ws = ghw5 dim

    //        let wf z = 
    //                    weights  //for each kth fixing
    //                    |> Vector.mapi( fun k w -> 
    //                       let fki = fk k z 
    //                       w *  fki 
    //                       )
    //        let wff z = 
    //                    wf z //for each kth fixing
    //                    |> Vector.mapi( fun k x -> 
    //                       x * F.[k]  
    //                       )

    //        let fn z1 z = //for each risk factor scenario
    //                    (wff z //for each kth fixing
    //                    |> Vector.mapi( fun k w -> 
    //                       w * exp(-0.5* V.[k,0]*V.[k,0] + V.[k,0]*z1)                
    //                       )
    //                    |> Vector.sum) - strike


    //        let difffn z1 z =
    //                    wff z  //for each kth fixing
    //                    |> Vector.mapi( fun k w -> 
    //                        w * exp( - 0.5 * V.[k,0]*V.[k,0] + V.[k,0]*z1)                
    //                        *V.[k,0])
    //                    |> Vector.sum 

    //        let roots = 
    //            zs 
    //            |> Array.map( fun x ->
    //                // fn is increasing in z1, linked to fixing0
    //                // adding cases for trivial k.
    //                let z = vector x
    //                let u = 10.
    //                let l = -10.
    //                (if ( fn u z <= 0. ) then u
    //                    elif ( fn l z >= 0.) then l
    //                    else 
    //                        RootFinding.RobustNewtonRaphson.FindRoot(
    //                            ( fun z1 -> fn z1 z),
    //                            ( fun z1 -> difffn z1 z),
    //                            l, u)) * -1.)
    //                        //-1E3, 1E3, 0.01, 1000,10) * -1.)
    //        let opt = 
    //            roots
    //            |> Array.mapi( fun i d -> 
    //                let z = vector zs.[i]
    //                let o = 
    //                    match callput with
    //                    | Call -> 
    //                        (wff z
    //                        |> Vector.mapi( fun k w -> w * normcdf( d + V.[k,0]))
    //                        |> Vector.sum )- strike * normcdf(d)                       
    //                    | Put ->
    //                        strike * normcdf(-d) - 
    //                            (wff z
    //                            |> Vector.mapi( fun k w -> w * normcdf( -d - V.[k,0])) 
    //                            |> Vector.sum )
    //                o * ws.[i])
    //            |> Array.sum
                            
    //        let deltas' = 
    //            weights 
    //            |> Vector.mapi( fun k w -> 
    //                roots
    //                |> Array.mapi( fun i d -> 
    //                    let z = vector zs.[i] 
    //                    let wf = fk k z
    //                    ws.[i] * w * wf * normcdf ( d + V.[k,0] ) )   
    //                |> Array.sum )            
    //        //just return total long/short delta, because after consolidatation, the current implementation did not 
    //        //trace back to original contracts. 
    //        let deltas = (match callput with | Call -> deltas' |Put -> deltas' - weights) |> Vector.toArray
    //        let delta1,delta2 = deltas |> Array.splitAt fw1.Count
    //        let delta1sum = Array.sum delta1
    //        let delta2sum = Array.sum delta2
    //        let delta = [|delta1sum ; delta2sum |]
    //        let fwderr = 
    //            weights 
    //            |> Vector.mapi( fun k _ -> 
    //                zs
    //                |> Array.mapi( fun i z' -> 
    //                    let z = vector z' 
    //                    let wf = fk k z
    //                    ws.[i] * (wf - 1.0))
    //                |> Array.sum )
                    
    //        let adj =
    //            let calladj = 
    //                deltas' 
    //                |> Vector.mapi( fun k d -> 
    //                    d * F.[k] * fwderr.[k])
    //                |> Vector.sum
    //            match callput with
    //            | Call  -> calladj
    //            | Put ->
    //                calladj - 
    //                    (weights 
    //                    |> Vector.mapi( fun k w -> w * F.[k] *fwderr.[k])
    //                    |> Vector.sum )

    //        (opt - adj), delta  //return deltas in long/short 2 elem array

    //Choi's general method using cov inputs
    let rec optionChoiG (f:Vector<float>) (w:Vector<float>) (sigma:Matrix<float>) strike callput (o:int list)=
        //validate inputs
        if strike < 0. then 
            let callput' = match callput with | Call -> Put | Put -> Call
            let (opt,delta) = optionChoiG f (w * -1.) sigma -strike callput' o //put equivalent
            opt, delta
        else
            if f.Count = 1 then // use bs
                let o = bs (f.[0]*w.[0]) strike (sqrt sigma.[0,0]) 1.0 callput 
                let delta = ( bsdelta (f.[0]*w.[0]) strike (sqrt sigma.[0,0]) 1.0 callput ) * w.[0]
                o, [|delta|]
            else 
            let V = getVChoi f w (fixCov sigma)
            //for each z_dot, find z1 and then C_bs, and then sum them using GH
            let n = f.Count 
            let fk k (z:Vector<float>) = //formula 7 
                let vk = V.Row(k) //kth row vector
                let vksum = vk * vk  - vk.[0] * vk.[0]
                exp( -0.5 * vksum + vk.SubVector(1,z.Count) * z ) //fk for some z
            //let e = V.Svd().S
            //let et = e.Sum()
            //let m = 
            //    e 
            //    |> Vector.toArray 
            //    |> Array.scan (fun t x -> t + x ) 0. 
            //    |> Array.map( fun x -> x / et) 
            //    |> Array.findIndex( fun x -> x > 0.9 ) 
            //let dim = min (n-1) ( min m (o.Length )) //cap at l levels
            let dim = min (n-1) ( o.Length ) //cap at l levels
            let zs,ws = gh o.[ 0 .. (dim - 1)] 
            let wf z = 
                        w  //for each kth fixing
                        |> Vector.mapi( fun k w -> 
                           let fki = fk k z 
                           w *  fki 
                           )
            let wff z = 
                        wf z //for each kth fixing
                        |> Vector.mapi( fun k x -> 
                           x * f.[k]  
                           )
            let fn z1 z = //for each risk factor scenario
                        (wff z //for each kth fixing
                        |> Vector.mapi( fun k w -> 
                           w * exp(-0.5* V.[k,0]*V.[k,0] + V.[k,0]*z1)                
                           )
                        |> Vector.sum) - strike
            let difffn z1 z =
                        wff z  //for each kth fixing
                        |> Vector.mapi( fun k w -> 
                            w * exp( - 0.5 * V.[k,0]*V.[k,0] + V.[k,0]*z1)                
                            *V.[k,0])
                        |> Vector.sum 
            let roots = 
                zs 
                |> Array.map( fun x ->
                    // fn is increasing in z1, linked to fixing0
                    // adding cases for trivial k.
                    let z = vector x
                    let u = 10.
                    let l = -10.
                    (if ( fn u z <= 0. ) then u
                        elif ( fn l z >= 0.) then l
                        else 
                            RootFinding.RobustNewtonRaphson.FindRoot(
                                ( fun z1 -> fn z1 z),
                                ( fun z1 -> difffn z1 z),
                                l, u)) * -1.)
            let opt = 
                roots
                |> Array.mapi( fun i d -> 
                    let z = vector zs.[i]
                    let o = 
                        match callput with
                        | Call -> 
                            (wff z
                            |> Vector.mapi( fun k w -> w * normcdf( d + V.[k,0]))
                            |> Vector.sum )- strike * normcdf(d)                       
                        | Put ->
                            strike * normcdf(-d) - 
                                (wff z
                                |> Vector.mapi( fun k w -> w * normcdf( -d - V.[k,0])) 
                                |> Vector.sum )
                    o * ws.[i])
                |> Array.sum
            let deltas' = 
                w
                |> Vector.mapi( fun k w -> 
                    roots
                    |> Array.mapi( fun i d -> 
                        let z = vector zs.[i] 
                        let wf = fk k z
                        ws.[i] * w * wf * normcdf ( d + V.[k,0] ) )   
                    |> Array.sum )            
            //just return total long/short delta, because after consolidatation, the current implementation did not 
            //trace back to original contracts. 
            let deltas = (match callput with | Call -> deltas' |Put -> deltas' - w) |> Vector.toArray
            let fwderr = 
                w
                |> Vector.mapi( fun k _ -> 
                    zs
                    |> Array.mapi( fun i z' -> 
                        let z = vector z' 
                        let wf = fk k z
                        ws.[i] * (wf - 1.0))
                    |> Array.sum )
            let adj =
                let calladj = 
                    deltas' 
                    |> Vector.mapi( fun k d -> 
                        d * f.[k] * fwderr.[k])
                    |> Vector.sum
                match callput with
                | Call  -> calladj
                | Put ->
                    calladj - 
                        (w
                        |> Vector.mapi( fun k w -> w * f.[k] *fwderr.[k])
                        |> Vector.sum )
            max 0. (opt - adj), deltas  //return deltas in input vector
    
    //Choi model with 4 dim and descretize 17/2
    //This is generally accurate within 0.1c err
    let optionChoi (f:Vector<float>) (w:Vector<float>) (sigma:Matrix<float>) strike callput =
        optionChoiG f w sigma strike callput [7;3;2]

    ///using cov inputs
    let optionChoi2AssetCov (f1:Vector<float>) (fw1:Vector<float>)  
        (f2:Vector<float>) (fw2:Vector<float>) k (sigma:Matrix<float>) callput o =
        //validate inputs
        //call general case
        let f = appendVector f1 f2
        let w = appendVector fw1 (fw2 * -1.) 
        let rho = sigma.[0,f.Count-1] / sqrt ( sigma.[0,0] * sigma.[f.Count-1,f.Count-1])
        let (opt,deltas) = 
            if rho > 0.7 then 
                let n = if k >= 0. then 0 else f.Count-1 
                //let n = if (sigma.[0,0] > sigma.[f.Count-1,f.Count-1]) then 0 else f.Count-1 
                //if k is on the same side of long or short.
                let k' =  -w.[n] * f.[n]
                let fn = f.[n]
                let sigma' = 
                    sigma |> Matrix.mapi ( fun i j v -> 
                        if i = n && j = n then
                            v
                        elif i = n || j = n then
                            -v + sigma.[n,n]
                        else
                            v - sigma.[i,n] - sigma.[j,n] + sigma.[n,n]) 
                //asset price is positive, weights can be either way
                if k = 0.0 then 
                    f.[n] <- 1E-10 //should really remove the factor when the price is 0 or too small, to avoid getV numerical error.
                    sigma' |> Matrix.mapiInPlace ( fun i j v -> 
                        if i = n && j = n then
                            1E-12 
                        elif i = n || j = n then
                            0.
                        else
                            v )
                elif k > 0. then
                    f.[n] <- k
                    w.[n] <- -1.0
                else
                    f.[n] <- -k
                    w.[n] <- 1.0
                let (opt,deltas) = optionChoiG f w sigma' k' callput o
                //todo check delta conversion
                let dn = ((vector deltas) * f  - opt ) / fn 
                deltas.[n] <- dn
                (opt,deltas) 
            else 
                optionChoiG f w sigma k callput o
        let delta1,delta2 = deltas |> Array.splitAt f1.Count
        let delta1sum = Array.sum delta1
        let delta2sum = Array.sum delta2
        let delta = [|delta1sum ; delta2sum |]
        opt, delta  //return deltas in long/short 2 elem array

    //spread option using Choi, without change of numeraire 
    let optionChoi2Asset (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) v2 k (rho:float) callput =
        let f = appendVector f1 f2
        let w = appendVector fw1 (fw2 * -1.) 
        let sigma = getCov t1 v1 t2 v2 rho
        let (opt,deltas) = optionChoi f w sigma k callput 
        let delta1,delta2 = deltas |> Array.splitAt f1.Count
        let delta1sum = Array.sum delta1
        let delta2sum = Array.sum delta2
        let delta = [|delta1sum ; delta2sum |]
        opt, delta  //return deltas in long/short 2 elem array

    ///assuming perferect correalation in asset
    let optionChoi2AssetG (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) v2 k (rho:float) callput o =
        let f = appendVector f1 f2
        let w = appendVector fw1 (fw2 * -1.) 
        let sigma = getCov t1 v1 t2 v2 rho
        let (opt,deltas) = optionChoiG f w sigma k callput o
        let delta1,delta2 = deltas |> Array.splitAt f1.Count
        let delta1sum = Array.sum delta1
        let delta2sum = Array.sum delta2
        let delta = [|delta1sum ; delta2sum |]
        opt, delta  //return deltas in long/short 2 elem array

    ///assuming perferect correalation in asset
    ///change of Numeraire to work with high correlation > 0.75 precision issue
    let optionChoi2AssetN (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) v2 k (rho:float) callput o =
        //validate inputs
        if Vector.exists (fun x -> x <= 0. ) t1 then invalidArg "t1'" "time to matuirty needs to be positive values"
        if Vector.exists (fun x -> x <= 0. ) t2 then invalidArg "t1'" "time to matuirty needs to be positive values"
        if Vector.exists (fun x -> x <= 0. ) v1 then invalidArg "v1'" "vol needs to be positive values"
        if Vector.exists (fun x -> x <= 0. ) v2 then invalidArg "v2'" "vol needs to be positive values"
        //call general case
        let sigma = getCov t1 v1 t2 v2 rho
        optionChoi2AssetCov f1 fw1 f2 fw2 k sigma callput o
        
    //asian Choi method
    let asianoptionChoi (f:Vector<float>) (w:Vector<float>) k (sigma:Matrix<float>) callput =
        let (opt,deltas) = optionChoi f w sigma k callput 
        let delta= Array.sum deltas
        opt, delta  //return deltas in long/short 2 elem array
