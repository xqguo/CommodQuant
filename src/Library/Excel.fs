module Excel

open System.IO
open NetOffice

NetOffice.Settings.Default.MessageFilter.Enabled <- true
NetOffice.Settings.Default.MessageFilter.RetryMode <- RetryMessageFilterMode.Delayed

/// Helper function to represent string or floating point cell content as a string.
let cellContent (range: ExcelApi.Range) =
    match range.Value2 with
    | :? string as _string -> _string
    | :? double as _double -> sprintf "%f" _double
    | _ -> ""

/// Helper function to return cell content as float if possible, if not as 0.0.
let cellDouble (range: ExcelApi.Range) =
    match range.Value2 with
    | :? double as _double -> _double
    | _ -> 0.0

/// Returns the specified worksheet range as a sequence of indvidual cell ranges.
let toSeq (range: ExcelApi.Range) =
    seq {
        for r in 1 .. range.Rows.Count do
            for c in 1 .. range.Columns.Count do
                let cell = range.Item(r, c)
                yield cell
    }

/// Returns the specified worksheet range as a sequence of indvidual cell ranges, together with a 0-based
/// row-index and column-index for each cell.
let toSeqrc (range: ExcelApi.Range) =
    seq {
        for r in 1 .. range.Rows.Count do
            for c in 1 .. range.Columns.Count do
                let cell = range.Item(r, c)
                yield r, c, cell
    }

/// Takes a sequence of individual cell-ranges and returns an Excel range representation of the cells
/// (using Excel 'union' representation - eg. "R1C1, R2C1, R5C4").
let toRange (workSheet: ExcelApi.Worksheet) (rangeSeq: seq<ExcelApi.Range>) =
    let csvSeq sequence =
        let result = sequence |> Seq.fold (fun acc x -> acc + x + ",") ""
        result.Remove(result.Length - 1)

    let rangeName = rangeSeq |> Seq.map (fun cell -> cell.Address) |> csvSeq
    workSheet.Range(rangeName)

/// Takes a function and an Excel range, and returns the results of applying the function to each individual cell.
let map (f: ExcelApi.Range -> 'T) (range: ExcelApi.Range) = range |> toSeq |> Seq.map f

/// Takes a function and an Excel range, and returns the results of applying the function to each individual cell,
/// providing 0-based row-index and column-index for each cell as arguments to the function.
let maprc (f: int -> int -> ExcelApi.Range -> 'T) (range: ExcelApi.Range) =
    range
    |> toSeqrc
    |> Seq.map (fun item ->
        match item with
        | (r, c, cell) -> f r c cell)

/// Takes a function and an Excel range, and applies the function to each individual cell.
let iter (f: ExcelApi.Range -> unit) (range: ExcelApi.Range) =
    range |> toSeq |> Seq.iter (fun cell -> f cell)

/// Takes a function and an Excel range, and applies the function to each individual cell,
/// providing 0-based row-index and column-index for each cell as arguments to the function.
let iterrc (f: int -> int -> ExcelApi.Range -> unit) (range: ExcelApi.Range) =
    range
    |> toSeqrc
    |> Seq.iter (fun item ->
        match item with
        | (r, c, cell) -> f r c cell)

/// Takes a function and an Excel range, and returns a sequence of individual cell ranges where the result
/// of applying the function to the cell is true.
let filter (f: ExcelApi.Range -> bool) (range: ExcelApi.Range) =
    range |> toSeq |> Seq.filter (fun cell -> f cell)

//// Connect to existing Excel
let tryExcel () =
    try
        //try to get current excel
        let excelApp = ExcelApi.Application.GetActiveInstance()
        excelApp.Visible <- true
        Ok excelApp
    with _ ->
        Error "Cannot get existing excel instance"

/// find existing workbook by name
let tryBook filename (excel: ExcelApi.Application) =
    try
        Ok(excel.Workbooks.[filename])
    with _ ->
        Error("Cannot find workbook  " + filename)

/// find existing workbook by name
let tryOpenBook workbookDir filename (excel: ExcelApi.Application) =
    let f = Path.Combine(workbookDir, filename)

    try
        //check path and file exists
        if (File.Exists(f)) then
            Ok(excel.Workbooks.Open f)
        else
            Error("Cannot find workbook  " + f)
    with _ ->
        Error("Cannot open workbook  " + f)

/// Get a reference to the workbook:
let trySheet sheet (book: ExcelApi.Workbook) =
    try
        Ok(book.Sheets.[sheet] :?> ExcelApi.Worksheet)
    with _ ->
        Error("Cannot find sheet " + sheet)

/// Get a reference to a named range:
let tryRange rangeName (sheet: ExcelApi.Worksheet) =
    try
        Ok(sheet.Range(rangeName))
    with _ ->
        Error("Cannot find range " + rangeName)

let tryRangeFull file sheet range =
    tryExcel ()
    |> Result.bind (tryBook file)
    |> Result.bind (trySheet sheet)
    |> Result.bind (tryRange range)

///A way to kill all running excel.exe processes
let killExcel () =
    let info = new System.Diagnostics.ProcessStartInfo()
    info.UseShellExecute <- false
    info.FileName <- "taskkill.exe"
    info.Arguments <- "/F /IM excel.exe"
    let p = System.Diagnostics.Process.Start(info)
    p.Close()

///start a new excel process
///Existing from the gui will make a hidden process
let tryStartExcel () =
    try
        let excel = new ExcelApi.Application()
        Ok excel
    with _ ->
        Error "Cannot start excel instance"

let quitExcel (excel: ExcelApi.Application) =
    excel.Quit()
    excel.Dispose()
    NetOffice.Core.Default.DisposeAllCOMProxies()

///take a 2d continous range and dump the text into csv file
let toCSV f (range: ExcelApi.Range) =
    //com call sometimes choke.
    let r = range.Rows.Count
    let data = map cellContent range |> Seq.splitInto r

    let content =
        data
        |> Seq.map (fun s ->
            let csvRow = Array.fold (fun acc x -> acc + x + ",") "" s
            csvRow.TrimEnd ',')
    //if File.Exists f then File.Delete f
    //let tmp = f+".tmp"
    //printfn "writing to %s" tmp
    File.WriteAllLines(f, content)
//printfn "moving to %s" f
//File.Move(tmp, f)
