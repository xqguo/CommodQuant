(**

Test Options using FsCheck
========================
*)

#I "../.."
#load "../Library/Options.fs"
#load "packages/MathNet.Numerics.FSharp/MathNet.Numerics.fsx"
#load ".paket\\load\\net461\\FsCheck.fsx"

open Options
open FsCheck
open MathNet.Numerics.LinearAlgebra

let ``test bs`` ( PositiveInt f )  ( PositiveInt k ) ( PositiveInt v ) ( PositiveInt t ) o  = 
    let f = float f / 100.
    let k = float k / 100.
    let v = float v / 100.
    let t = float t / 365.
    let p =  (bs f k v 0. t o ) 
    p >= 0. |@ sprintf "Option value is positive %f" p
Check.Quick ``test bs``

let ``test spread`` ( PositiveInt f)  ( PositiveInt k)= 
    let f1 = Vector.Build.Dense(8, float f)
    let f2 = (( [ 40; 41; 42; 43; 44; 47; 48; 49 ] |> List.map float |> vector ) + 0.01 )/ 365. //fixing dates
    let v1 = Vector.Build.Dense(8, 0.25) // vol for each fixing
    let fw1 = Vector.Build.Dense( 8, 1./8. ) //weights longside

    let f2 = Vector.Build.Dense(8, 0.0001) // forwards short side
    let t2 = f2 //fixing dates
    let v2 = Vector.Build.Dense(8, 0.0001) // vol for each fixing 
    let fw2 = fw1 //weights longside

    let p1 = Vector.Build.Dense(8, 0.0) //  #past fixing longside
    let pw1 = Vector.Build.Dense(8, 0.0)
    let p2 = Vector.Build.Dense(8, 0.0)
    let pw2 = Vector.Build.Dense(8, 0.0)

    let k = float k //strike
    let rho = 0. //correlation between long/short fixing
    let callput = 1.
    let so = spreadoption f1 fw1 f2 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
    let ao = asianoption f1 fw1 f2 v1 k 'c' 0.
    abs ( so - ao ) < 0.001 |@ sprintf "spread option and asian option price are close: %f, %f" so ao

Check.Quick ``test spread``
