module TestOptions
open FsCheck.Xunit
open Options
open FsCheck
open FsCheckTypes
open MathNet.Numerics.LinearAlgebra


[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``generated positive floats should be positive`` x = x > 0.0
//Check.Verbose ``generated positive floats should be positive``

[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test bs`` f  k v t = 
    let c = (bs f k v 0. t Call)
    let p = (bs f k v 0. t Put)
    let y = f - k 
    let diff = c - p - y
    let c2 = (bs (f+0.001) k v 0. t Call)
    let delta = (c2 - c) / 0.001
    (p >= 0.0 ) |@ sprintf "Option value is non-negative" .&.
    (abs(diff) <= 0.001)  |@ sprintf "Must satisfy call put parity %f" diff .&. 
    (delta >= 0.0) |@ sprintf "Call option delta is positive"

[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test spread`` f k = 
    let f1 = DenseVector.create 8 f
    let t1 = (( [ 40; 41; 42; 43; 44; 47; 48; 49 ] |> List.map float |> vector ) + 0.01 )/ 365. //fixing dates
    let v1 = Vector.Build.Dense(8, 0.25) // vol for each fixing
    let fw1 = Vector.Build.Dense( 8, 1./8. ) //weights longside

    let f2 = Vector.Build.Dense(8, 0.0001) // forwards short side
    let t2 = t1 //fixing dates
    let v2 = Vector.Build.Dense(8, 0.0001) // vol for each fixing 
    let fw2 = fw1 //weights longside

    let p1 = Vector.Build.Dense(8, 0.0) //  #past fixing longside
    let pw1 = Vector.Build.Dense(8, 0.0)
    let p2 = Vector.Build.Dense(8, 0.0)
    let pw2 = Vector.Build.Dense(8, 0.0)

    let rho = 0. //correlation between long/short fixing
    let callput = 1.
    let so = spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
    let ao = asianoption f1 fw1 t1 v1 k Call 0.
    abs ( so - ao ) < 0.001 |@ sprintf "spread option and asian option price are close: %f, %f" so ao
