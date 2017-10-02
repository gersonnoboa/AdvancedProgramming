(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Working with data: intro to type providers and charting

  ------------------------------------
  Name: Luis Noboa
  TUT Student ID: lunobo
  ------------------------------------


  Answer the questions below.  You answers to questions 1--6 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 29, 2016.
*)

// 1) The following three different fuel consumption units for vehicles are in use:
//    * litres per 100 km
//    * miles per imperial gallon (in use in the UK)
//    * miles per US gallon (in use in the US)
// 1.a) Define the units in terms of units of measure.
// 1.b) Define 2 functions that convert the appropriate US and imperial mpg values to
//      litres per 100 km. 
// 1.c) Define a function that converts litres per 100 km of appropriate fuel to
//      CO2 emissions g per km.

[<Measure>] type km
[<Measure>] type hundredkm
[<Measure>] type mile
[<Measure>] type litre
[<Measure>] type iGallon
[<Measure>] type uGallon

let usGallon = 3.785411784<litre/uGallon>
let impGallon = 4.54609188<litre/iGallon>
let mile = 1.609344
let mileHundred = 1.0<hundredkm/mile> * (mile/100.0)

let convertUmpgToLpk (umpg: float<mile/uGallon>) = (usGallon) / (mileHundred * umpg)
convertUmpgToLpk 1.0<mile/uGallon>
convertUmpgToLpk 45.0<mile/uGallon>

let convertImpgToLpk (impg: float<mile/iGallon>) = (impGallon) / (mileHundred * impg)
convertImpgToLpk 1.0<mile/iGallon>
convertImpgToLpk 48.0<mile/iGallon>

[<Measure>] type co2g
let convertLpkToCo2 (lpk: float<litre/hundredkm>) = lpk * 23.2<co2g hundredkm km/litre>
convertLpkToCo2 2.0<litre/hundredkm>
// 2) Get the fuel consumption data
// 2.a) in imperial MPG (miles per imperial gallon) of at least 20 vehicles from
// http://carfueldata.direct.gov.uk/search-by-fuel-economy.aspx
// Save the data in file called imperial.csv

// 2.b) Get the fuel consumption data of at least 20 cars in US MPG (miles per US gallon) from
// https://www.fueleconomy.gov/feg/download.shtml
// save the data in file called us.csv

// 3) Load the imperial.csv and us.csv files using FSharp.Data.CsvProvider<>

#r @"..\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
#r @"..\packages\FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"
#load @"..\packages\FSharp.Charting.0.90.14\FSharp.Charting.fsx"

open FSharp.Data
open FSharp.Charting
open FSharp.Charting.ChartTypes
open FSharp.Data.CsvExtensions

let imperialCsv = CsvFile.Load(__SOURCE_DIRECTORY__ + "\imperial.csv").Cache()
let usCsv = CsvFile.Load(__SOURCE_DIRECTORY__ + "\us.csv").Cache()

// 4) Write a function to convert the appropriate mpg data into
//    litres per 100 km using the functions defined in Q1.

let convert (csv: Runtime.CsvFile<CsvRow>) (t: string) =
    match t with
    | "i" -> Seq.map (fun (x:CsvRow) -> (x.["Model"], convertImpgToLpk(float(x.["Imperial combined"]) * 1.0<mile/iGallon>))) csv.Rows
    | _ -> Seq.map (fun (x:CsvRow) -> (x.["Carline"], convertUmpgToLpk(float(x.["Comb FE (Guide) - Conventional Fuel"]) * 1.0<mile/uGallon>))) csv.Rows

convert imperialCsv "i"
convert usCsv "u"

// 5) Display the converted data in an appropriate chart (select the type that is most 
//    appropriate for displaying the data).

let chart = 
    (convert imperialCsv "i")
    |> Chart.Column 
    |> Chart.WithXAxis (LabelStyle = ChartTypes.LabelStyle(Angle = -45, Interval = 1.0), Title = "Models") 
    |> Chart.WithYAxis (Title = "Litres per 100km")
    |> Chart.WithTitle "Imperial Fuel Efficiency"

let chart2 = 
    (convert usCsv "u")
    |> Chart.Column
    |> Chart.WithXAxis (LabelStyle = ChartTypes.LabelStyle(Angle = -45, Interval = 1.0), Title = "Models") 
    |> Chart.WithYAxis (Title = "Litres per 100km")
    |> Chart.WithTitle "US Fuel Efficiency"
// 6) Combine the data from 2 data sources into a single chart. Add appropriate titles and
//    legends. 

let chart3 = 
    Seq.append (convert imperialCsv "i") (convert usCsv "u")
    |> Chart.Column
    |> Chart.WithXAxis (LabelStyle = ChartTypes.LabelStyle(Angle = -45, Interval = 1.0), Title = "Models") 
    |> Chart.WithYAxis (Title = "Litres per 100km")
    |> Chart.WithTitle "Fuel Efficiency"


List.collect (fun x -> [x*x]) [2;3;4]