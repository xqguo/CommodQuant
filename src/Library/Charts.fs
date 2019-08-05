namespace Commod
module Charts = 
    open XPlot.GoogleCharts
    open MathNet.Numerics.LinearAlgebra

    let plotLines (m:Matrix<'u>) = 
        m
        |> Matrix.toRowArrays
        |> Array.map( fun v -> v |> Array.mapi( fun i x -> (i,x) ))
        |> Chart.Line
        |> Chart.Show

    let plotTable (m:Matrix<'u>) = 
        m
        |> Matrix.toRowArrays
        |> Array.map( fun v -> v |> Array.mapi( fun i x -> (i,x) ))
        |> Chart.Table
        |> Chart.Show

    let scatter (m:Matrix<'u>) = 
        let options =
            Options(
                title = "Scatter",
                hAxis =
                    Axis(
                        title = "X"
                    ),
                vAxis = 
                    Axis(
                        title = "Y"),
                width = 1000, 
                height = 1000
            )
        //plot 1st row vs 2nd row
        let arr = m |> Matrix.toRowArrays
        (arr.[0], arr.[1])
        ||> Array.zip
        |> Chart.Scatter
        |> Chart.WithOptions options
        |> Chart.Show

