namespace Commod
[<AutoOpen>]
module Options =
    open System
    open MathNet.Numerics.Distributions
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Integration

    type Payoff = 
        | Call 
        | Put 

    let normcdf = fun x -> Normal.CDF ( 0., 1., x )  
    let normpdf = fun x -> Normal.PDF ( 0., 1., x )  
    ///returns black scholes price
    let bs f k v r t (o:Payoff) = 
        if ( f<=0. || k <= 0. || v<=0. || t<=0. ) then 
            let iv = 
                match o with 
                | Call -> f - k  
                | Put -> k - f  
            exp(-r*t) * (max iv 0.)
        else
            let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
            let d2 = d1 - v*sqrt(t)
            match o with
            | Call -> exp(-r*t)*(f*normcdf(d1)-k*normcdf(d2))
            | Put -> exp(-r*t)*(k*normcdf(-d2)-f*normcdf(-d1))

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
        (x1, x11, 0. )

    ///cross moments
    let momentsx (f1w:Vector<float>) (v1:Vector<float>) (t1:Vector<float>) (f2w:Vector<float>) (v2:Vector<float>) (t2:Vector<float>) rho = 
        let ff = f1w.OuterProduct f2w
        if (rho = 0.) then 
            ff.RowSums().Sum() //all cross terms are 0.
        else
            let tmatrix = getTmatrix t1 t2
            //((f1w.OuterProduct f2w).*exp( (v1.OuterProduct v2) .* tmatrix * rho )) |> sum
            ((f1w.OuterProduct f2w).*exp( (v1.OuterProduct v2) .* tmatrix  )) |> sum

    let asianoption (f1:Vector<float>) (fw1:Vector<float>) t1 v1 k' o p1w =
        let (y1, y11, delta) = moments (f1 .* fw1) v1 t1  
        let k = k' - p1w - delta
        let v = sqrt( log (y11/y1/y1) )
        bs y1 k v 0. 1. o 

    ///spread option pricing using numerical integration
    let rec spreadoption (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) v2 k rho callput 
        (p1:Vector<float>) (pw1:Vector<float>) (p2:Vector<float>) (pw2:Vector<float>)= 
            let f1w = f1 .* fw1 //1st asset weighted
            let f2w = f2 .* fw2
            let k' = k - (p1 .* pw1).Sum() + (p2 .* pw2 ).Sum() // adapte K for past fixings
            if k' < 0. then
                let v0 = Vector<float>.Build.Dense(1) 
                spreadoption f2 fw2 t2 v2 f1 fw1 t1 v1 -k' rho -callput v0 v0 v0 v0 //put equivalent
            else 
            //moments
                let ( x1, x11, _) = moments f1w v1 t1
                let ( x2, x22, _) = moments f2w v2 t2
                let x12 = momentsx f1w v1 t1 fw2 v2 t2 rho
            //#intermediates
                let b1 = sqrt(log(x22 / x2 / x2 ))
                let b2 = 1. / b1 * log(x12 / (x1 * x2))
                // let g = callput / sqrt(log(x11 / x1 / x1) - b2 * b2) //this can be nan? 
                let g = callput / sqrt(max (log(x11 / x1 / x1) - b2 * b2) 1E-12) //this can be nan? 
                let i1 = Func<float, float> (fun x -> normpdf(x) * normcdf(g * log(sqrt(x11) * exp(b2 * x) / (k + x2 * x12 / (x1 * sqrt(x22)) * exp(b1 * x)))))
                let i2 = Func<float, float>(fun x -> normpdf(x) * normcdf(g * log(x1 * x12 / (x2 * sqrt(x11)) * exp(b2 * x) / (k + sqrt(x22) * exp(b1 * x)))))
                let i3 = Func<float,float>( fun x -> normpdf(x) * normcdf(g * log(x1 * x1 / sqrt(x11) * exp(b2 * x) / (k + x2 * x2 / sqrt(x22) * exp(b1 * x)))))
                let stddevs = 6.0
                let partitions = 100000
                let i1' = SimpsonRule.IntegrateComposite( i1, -stddevs, stddevs, partitions)
                let i2' = SimpsonRule.IntegrateComposite( i2, -stddevs, stddevs, partitions)
                let i3' = SimpsonRule.IntegrateComposite( i3, -stddevs, stddevs, partitions)
                callput * ( x1 * i1' - x2 * i2' - k * i3')