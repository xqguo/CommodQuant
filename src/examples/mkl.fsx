// #r "nuget: MathNet.Numerics"
// #r "nuget: MathNet.Numerics.MKL.Win" // Or appropriate MKL package for your OS (Linux, OSX)


#r "nuget: MathNet.Numerics, 6.0.0-beta1"
#r "nuget: MathNet.Numerics.MKL.Win-x64"
#r "nuget: MathNet.Numerics.Providers.MKL, 6.0.0-beta1"

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open System
open System.Diagnostics



let measurePerformance (action: unit -> unit) (iterations: int) =
    let stopwatch = new Stopwatch()
    let mutable totalTime = 0L

    for i in 1..iterations do
        stopwatch.Start()
        action()
        stopwatch.Stop()
        totalTime <- totalTime + stopwatch.ElapsedMilliseconds
        stopwatch.Reset()

    let averageTime = float totalTime / float iterations
    printfn $"Average time: {averageTime} ms"

// Function to run the test with a given provider setup
let runTest (testName: string) (setupAction: unit -> unit) (matrixSize: int) (iterations: int) =
    printfn $"\n--- {testName} ---"
    setupAction() // Setup the provider (e.g., UseNativeMKL)

    // Create random matrices (using float for performance in this example)
    let matrixA = Matrix<float>.Build.Random(matrixSize, matrixSize)
    let matrixB = Matrix<float>.Build.Random(matrixSize, matrixSize)

    measurePerformance (fun () -> matrixA * matrixB |> ignore) iterations

// Main execution
let matrixSize = 2000 // Adjust as needed
let iterations = 5

// Run tests
runTest "Default"  (fun () -> () ) matrixSize iterations // No setup action for managed
runTest "Managed Provider"  Control.UseManaged matrixSize iterations // No setup action for managed
runTest "MKL Provider" Control.UseNativeMKL matrixSize iterations

System.Console.ReadKey() |> ignore