namespace Commod
[<AutoOpen>]
module Options =
    open System
    open MathNet.Numerics
    open MathNet.Numerics.Distributions
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Integration
    
    type Payoff = 
        | Call 
        | Put 

    let normcdf = fun x -> Normal.CDF ( 0., 1., x )  
    let normpdf = fun x -> Normal.PDF ( 0., 1., x )  
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

    ///bs fwd delta
    let bsdelta f k v t (o:Payoff) = 
        if ( f<=0. || k <= 0. || v<=0. || t<=0. ) then 
            match o with 
            | Call -> if f >= k  then 1.0 else 0.0
            | Put ->  if f <= k  then 1.0 else 0.
        else
            let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
            let d2 = d1 - v*sqrt(t)
            match o with
            | Call -> normcdf(d1)
            | Put -> -normcdf(-d1)
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

    ///cross moments
    let momentsx (f1w:Vector<float>) (v1:Vector<float>) (t1:Vector<float>) (f2w:Vector<float>) (v2:Vector<float>) (t2:Vector<float>) rho = 
        let ff = f1w.OuterProduct f2w
        if (rho = 0.) then 
            ff |> sum //all cross terms are 0.
        else
            let tmatrix = getTmatrix t1 t2
            (ff .* ((v1.OuterProduct v2).*tmatrix*rho).PointwiseExp()) |> sum 

    //asian fwd price moment matching method
    let asianoption (f1:Vector<float>) (fw1:Vector<float>) t1 v1 k' o p1w =
        let (y1, y11, delta) = moments (f1 .* fw1) v1 t1  
        let k = k' - p1w - delta
        let v = sqrt( log (y11/y1/y1) )
        bs y1 k v 1. o 

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

    let rec ghz5 dim =     
        let s2 = sqrt 2.0
        let z3 = [|-2.02018287; -0.95857246;  0.; 0.95857246;  2.02018287|] |> Array.map ( fun x -> x* s2)

        match dim with
        | x when x <= 0 -> invalidArg "dim" "Dim should be an int >= 1"
        | 1 ->     
            [| for x in z3 do yield [|x|] |]
            //let w3 = [0.2954089752; 1.1816359006 ; 0.2954089752] |> List.map (*) cons
        | _ -> 
            permutate z3 (ghz5 (dim-1)) //previous layer)

    let rec ghw5 dim =     
        let cons = 1.0/sqrt(System.Math.PI)
        let w3 = [|0.01995324; 0.39361932; 0.94530872; 0.39361932; 0.01995324|] |> Array.map ( fun x -> x * cons )
        match dim with
        | x when x <= 0 -> invalidArg "dim" "Dim should be an int >= 1"
        | 1 -> w3
        | _ -> permprod w3 (ghw5 (dim-1))
    //recursive version with dim input
    //https://en.wikipedia.org/wiki/Gauss%E2%80%93Hermite_quadrature
    // 1 <= dim 
    let s2 = sqrt 2.0
    let cons = sqrt(System.Math.PI)
    let rec ghz3 dim =     
        let z3 = [|-1.2247448714;  0. ;1.2247448714|] |> Array.map ( fun x -> x* s2)
        match dim with
        | x when x <= 0 -> invalidArg "dim" "Dim should be an int >= 1"
        | 1 ->     
            [| for x in z3 do yield [|x|]|]
            //let w3 = [0.2954089752; 1.1816359006 ; 0.2954089752] |> List.map (*) cons
        | _ -> 
            permutate z3 (ghz3 (dim-1)) //previous layer)

    let rec ghw3 dim =     
        let w3 = [|0.2954089752; 1.1816359006 ; 0.2954089752|] |> Array.map ( fun x -> x / cons )
        match dim with
        | x when x <= 0 -> invalidArg "dim" "Dim should be an int >= 1"
        | 1 -> w3
        | _ -> permprod w3 (ghw3 (dim-1))

    //let ghw = ghw5
    //let ghz = ghz5
    ///get V with Choi's method for 2 assets with constant correlation.
    let getVChoi (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) (v2:Vector<float>) (rho:float) = 
            let f1w = f1 .* fw1 //long side weighted
            let f2w = f2 .* fw2 * -1. //short side weighted
            let w = appendVector fw1 (fw2 * -1.)
            let v = appendVector v1 v2 
            let n = f1.Count + f2.Count
            //assuming constant correlation between 2 assets.
            let g' = appendVector f1w f2w  
            let g = (g'/ (g'.L2Norm()))
            let sigma12 = getSigma2 v1 t1 v2 t2 rho
            let sigma11 = getSigma2 v1 t1 v1 t1 1.0
            let sigma22 = getSigma2 v2 t2 v2 t2 1.0
            //printfn "sigma12 dims: %i %i" sigma12.RowCount sigma12.ColumnCount
            //printfn "sigma11 dims: %i %i" sigma11.RowCount sigma11.ColumnCount
            //printfn "sigma22 dims: %i %i" sigma22.RowCount sigma22.ColumnCount
            let sigma = sigma11.Append( sigma12 ).Stack((sigma12.Transpose().Append(sigma22)))
            //printfn "%A" sigma
            let c = sigma.Cholesky().Factor //cholesky
            //let den2 = g.ToRowMatrix() * sigma * g.ToColumnMatrix() //should be a scalar 
            let den = sqrt(( g.ToRowMatrix() * sigma * g.ToColumnMatrix()).Item(0,0)) //should be a scalar 
            let Q1 = (c.Transpose() * g.ToColumnMatrix() ) /den
            let V1 = (c * Q1)// need to adjust so that w_k V_k1 > 0
            //printfn "v1: %A" (V1 |> Matrix.toArray2)
            let eps = 0.01 //choose this following Choi          
            let V1' = DenseVector.init n ( fun n -> 
                if V1.[n,0] * w.[n] > 0. 
                then V1.[n,0]
                else
                   let s = if w.[n] > 0. then 1.0 else -1.0
                   eps * s * v.[n] )       
            //let a = V1.L2Norm()
            //let b = V1'.L2Norm()
            let mu = (c.Inverse() * V1').L2Norm()
            //let mu = V1.L2Norm() / V1'.L2Norm()
            let V1'' = V1' / mu
            //let mu' = (c.Inverse() * V1'').L2Norm()
            //let mu = 0.99 //how to choose this? need to maintain norm same as vol1
            //V1 |> Matrix.toArray2
            //V1'.L2Norm()
            let Q1' = c.Inverse() * V1''
            let R = householderR Q1'
            let CR = c * R 
            let CR' = CR.SubMatrix(0, n, 1, n-1) //drop 1st column.
            let svd = CR'.Svd()      
            //let qdot = svd.VT
            let V = V1''.ToColumnMatrix().Append(     svd.U * svd.W)
            //let Q'= DenseMatrix.init n n ( fun i j -> 
            //    match i,j with
            //    | 0, 0 -> 1.0
            //    | 0, _ -> 0.0
            //    | _, 0 -> 0.0
            //    | _ , _ -> qdot.[i-1,j-1]
            //    ) 
            //let V = c* R* Q' //V
            //let test = (V * V.Transpose()) //is input covariance matrix
            V

    let consolidateInputs (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) =
        //consolidate future details to group same fixing dates
        //but retian the same weights, so that the delta still works
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

    ///Choi method of analytical 1 layer + GH remianing.
    ///assuming perferect correalation in asset
    let rec optionChoi (f1':Vector<float>) (fw1':Vector<float>) (t1':Vector<float>) (v1':Vector<float>) 
        (f2':Vector<float>) (fw2':Vector<float>) (t2':Vector<float>) v2' k' (rho:float) callput 
        (p1:Vector<float>) (pw1:Vector<float>) (p2:Vector<float>) (pw2:Vector<float>) = 
        let f1,fw1,t1,v1 = consolidateInputs f1' fw1' t1' v1'
        let f2,fw2,t2,v2 = consolidateInputs f2' fw2' t2' v2' 
        let strike = k' - (p1 .* pw1).Sum() + (p2 .* pw2 ).Sum() // adapte K for past fixings
        if strike < 0. then 
            let v0 = Vector<float>.Build.Dense(1) 
            let callput' = match callput with | Call -> Put | Put -> Call
            let (opt,delta) = optionChoi f2 fw2 t2 v2 f1 fw1 t1 v1 -strike rho callput' v0 v0 v0 v0 //put equivalent
            opt, (delta |> Array.rev) //delta sequence reverted.
        else
            let weights = appendVector fw1 (-1. * fw2)
            let V = getVChoi f1 fw1 t1 v1 f2 fw2  t2 v2 rho
            //for each z_dot, find z1 and then C_bs, and then sum them using GH
            let n = f1.Count + f2.Count
            let fk k (z:Vector<float>) = //formula 7 
                let vk = V.Row(k) //kth row vector
                let vksum = vk * vk  - vk.[0] * vk.[0]
                exp( -0.5 * vksum + vk.SubVector(1,z.Count) * z ) //fk for some z

            let F = appendVector f1 f2 
            let dim = min (n-1) 4 //cap at 5 levels
            if dim = 0 then failwith "degenerated bs formula directly, not implemented yet"
            let zs = ghz5 dim 
            let ws = ghw5 dim

            let wf z = 
                        weights  //for each kth fixing
                        |> Vector.mapi( fun k w -> 
                           let fki = fk k z 
                           w *  fki 
                           )
            let wff z = 
                        wf z //for each kth fixing
                        |> Vector.mapi( fun k x -> 
                           x * F.[k]  
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
                            //-1E3, 1E3, 0.01, 1000,10) * -1.)
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
                weights 
                |> Vector.mapi( fun k w -> 
                    roots
                    |> Array.mapi( fun i d -> 
                        let z = vector zs.[i] 
                        let wf = fk k z
                        ws.[i] * w * wf * normcdf ( d + V.[k,0] ) )   
                    |> Array.sum )            

            let deltas = (match callput with | Call -> deltas' |Put -> deltas' - weights) |> Vector.toArray

            let fwderr = 
                weights 
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
                        d * F.[k] * fwderr.[k])
                    |> Vector.sum
                match callput with
                | Call  -> calladj
                | Put ->
                    calladj - 
                        (weights 
                        |> Vector.mapi( fun k w -> w * F.[k] *fwderr.[k])
                        |> Vector.sum )

            (opt - adj), deltas  //return deltas same dimension as f1 f2 combined.

