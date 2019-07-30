#load "../../.paket/load/net472/XPlot.GoogleCharts.fsx"
#load "../../.paket/load/net472/XPlot.GoogleCharts.Deedle.fsx"
#load "../../.paket/load/net472/Deedle.fsx"
open XPlot.GoogleCharts
open System
open Deedle

let kepler =
    [
        DateTime(2314, 3, 15), 12400, "", ""
        DateTime(2314, 3, 16), 24045, "Lalibertines", "First encounter"
        DateTime(2314, 3, 17), 35022, "Lalibertines", "They are very tall"
        DateTime(2314, 3, 18), 12284, "Lalibertines", "Attack on our crew!"
        DateTime(2314, 3, 19), 8476, "Lalibertines", "Heavy casualties"
        DateTime(2314, 3, 20), 0, "Lalibertines", "All crew lost"
    ]
 
let gliese =
    [
        DateTime(2314, 3, 15), 10645, "", ""
        DateTime(2314, 3, 16), 12374, "", ""
        DateTime(2314, 3, 17), 15766, "Gallantors", "First Encounter"
        DateTime(2314, 3, 18), 34334, "Gallantors", "Statement of shared principles"
        DateTime(2314, 3, 19), 66467, "Gallantors", "Mysteries revealed"
        DateTime(2314, 3, 20), 79463, "Gallantors", "Omniscience achieved"
    ]

(**
Google Annotation Chart
=======================
[Full source and data](https://github.com/fslaborg/XPlot/blob/master/docsrc/content/chart/google-annotation-chart.fsx)
*)
(*** define-output:annotation ***)
let options = Options(displayAnnotations = false)
 
[kepler; gliese]
|> Chart.Annotation
|> Chart.WithOptions options
|> Chart.WithLabels
    [
        "Kepler-22b mission"; "Kepler-22b title"; "Kepler-22b text"
        "Gliese 163 mission"; "Gliese title"; "Gliese text"            
    ]
|> Chart.Show
(*** include-it:annotation ***)

let sales = kepler |> List.map( fun (k,v,_,_) -> k, float v)
let expenses = gliese |> List.map( fun (k,v,_,_) -> k ,float v)

frame ["sales" => series sales ; "exp" => series expenses] |> Chart.Calendar
frame ["sales" => series sales ] |> Chart.Calendar

let data =
    [
        "Mon", 20, 28, 38, 45
        "Tue", 31, 38, 55, 66
        "Wed", 50, 55, 77, 80
        "Thu", 77, 77, 66, 50
        "Fri", 68, 66, 22, 15        
        "Sat", 15, 66, 22, 68        
    ]
Chart.Candlestick data        

let pop =
  [ 
    "United States", 300
    "Japan", 500
  ]
pop
|> Chart.Geo

let data' =
    [
        "China", "China: 1,363,800,000"
        "India", "India: 1,242,620,000"
        "US", "US: 317,842,000"
        "Indonesia", "Indonesia: 247,424,598"
        "Brazil", "Brazil: 201,032,714"
        "Pakistan", "Pakistan: 186,134,000"
        "Nigeria", "Nigeria: 173,615,000"
        "Bangladesh", "Bangladesh: 152,518,015"
        "Russia", "Russia: 146,019,512"
        "Japan", "Japan: 127,120,000"
    ]
 
data'
|> Chart.Table
|> Chart.WithHeight 420