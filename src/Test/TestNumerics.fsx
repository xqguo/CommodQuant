#load "C:/Users/guanxinxin/source/repos/RegTest/.paket/load/net472/MathNet.Numerics.fsx"
#load "../../.paket/load/net472/MathNet.Numerics.FSharp.fsx"

open MathNet.Numerics
open MathNet.Numerics.Statistics
open MathNet.Numerics.Integration

open MathNet.Numerics.LinearAlgebra
//let m : Matrix<float> = DenseMatrix.randomStandard 50 50
//(m * m.Transpose()).Determinant()

SpecialFunctions.Gamma( 0.5)
let m = matrix [[ 1.0; 2.0 ]
                [ 3.0; 4.0 ]]
#time
let m' = m.Inverse()
let m'' = m'.ToArray()

let r = System.Random(0)
let normal = Distributions.Normal.WithMeanVariance(0., 1., r)
normal.Samples() |> Seq.take 5

let means = matrix [ [0.0];[0.0]]
let vars = matrix [[1. ;0.8];[0.8; 1.]]
let dims = matrix [[1.]]

let mnormal = Distributions.MatrixNormal(means, vars, dims)

mnormal.RandomSource <- r
let randomgen = 
    Seq.initInfinite int
    |> Seq.map( fun _ -> 
        let sample = mnormal.Sample()
        sample.Enumerate() |> Seq.toList )

let mr = randomgen |> Seq.take 100000 |> Seq.toList |> matrix
let stats = Statistics.Covariance (( mr.Column 0 ), (mr.Column 1 ))
#time
NewtonCotesTrapeziumRule.IntegrateComposite( (fun x -> (max  (100. * (exp x ) - 100.) 0. ) *(Distributions.Normal.PDF ( 0.0, 1.0, x))), -10.0, 10.0, 1000000)
SimpsonRule.IntegrateComposite( (fun x -> (max  (100. * (exp x ) - 100.) 0. ) *(Distributions.Normal.PDF ( 0.0, 1.0, x))), -10.0, 10.0, 1000)
NewtonCotesTrapeziumRule.IntegrateComposite( (fun x -> (max  (100. * (exp x ) - 100.) 0. ) *(Distributions.Normal.PDF ( 0.0, 1.0, x))), -10.0, 10.0, 1000)
GaussLegendreRule.Integrate( (fun x -> (max  (100. * (exp x ) - 100.) 0. ) *(Distributions.Normal.PDF ( 0.0, 1.0, x))), -10.0, 10.0, 10000)