namespace Commod
open System.Globalization
open System
open System.IO
open System.Threading.Tasks

[<AutoOpen>]
module Utils = 
    open FSharp.Data
    open FSharpx.Control
    open Microsoft.FSharp.Reflection

    let toString (x:'a) = 
        let (case, _ ) = FSharpValue.GetUnionFields(x, typeof<'a>)
        case.Name

    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None

    let culture = CultureInfo("en-GB")
    culture.Calendar.TwoDigitYearMax <- 2080
    let Error = Result.Error //avoid name clash with FSharpx
    let (+/) path1 path2 = Path.Combine(path1, path2)

    let tryParseWith tryParseFunc x = 
      match x with
      | "" | "NaN" | "N/A" -> None
      | _ ->  
        match tryParseFunc x with
        | true, v    -> Some v
        | false, _   -> None
    
    let parseDate   = tryParseWith System.DateTime.TryParse
    let parseInt    = tryParseWith System.Int32.TryParse
    let parseSingle = tryParseWith System.Single.TryParse
    let parseDouble = tryParseWith System.Double.TryParse
    let parseDouble10 = tryParseWith System.Double.TryParse >> Option.map (sprintf "%.10g") 
    let parseMMddyy s = DateTime.ParseExact(s,"MM/dd/yy", CultureInfo.InvariantCulture)
    //DateTime.Parse("12/1/2021", CultureInfo.InvariantCulture, DateTimeStyles.None)
    //DateTime.Parse("13/1/2021", culture)
    //DateTime.Parse("Sep21", culture)
    //DateTime.Parse("2011-02", culture)

    let datestr (str:string) = (str.ToUpper()) |> String.filter Char.IsLetterOrDigit //ignores separators like - /

    //let parseDateExact format str= 
    //    try 
    //        Some( System.DateTime.ParseExact( (datestr str), format, culture) )
    //    with
    //    | _ -> None

    let parseDateExact (format:string) str= 
          let (s,d) = System.DateTime.TryParseExact( (datestr str), format, culture, DateTimeStyles.None)
          if s then Some d else None

    // active patterns for try-parsing strings
    let (|YYYYMMDD|_|)   = parseDateExact "yyyyMMdd"
    let (|MMMYY|_|)   = parseDateExact "MMMyy"
    let (|DDMMMYY|_|)   = parseDateExact "ddMMMyy"
    let (|MMDDYY|_|)   = parseDateExact "MMddyy"
    let (|Date|_|)   = parseDate
    let (|Int|_|)    = parseInt
    let (|Single|_|) = parseSingle
    let (|Double10|_|) = parseDouble10
    let (|Double|_|) = parseDouble

    type Async with
        static member AwaitPlainTask (task : Task) =
            task.ContinueWith(ignore) |> Async.AwaitTask

    [<Literal>]
    let DEFAULT_BUFFER_SIZE = 4096

    let tryFile f = 
        if File.Exists(f) then 
            Some f
        else
            None

    let copyToAsync source dest =
      async {
          use sourceFile = new FileStream(source, FileMode.Open, FileAccess.Read, FileShare.Read, DEFAULT_BUFFER_SIZE, true);
          use destFile = new FileStream(dest, FileMode.OpenOrCreate, FileAccess.Write, FileShare.ReadWrite, DEFAULT_BUFFER_SIZE, true);
          do! sourceFile.CopyToAsync(destFile) |> Async.AwaitPlainTask
      }        

    let writeTextAsync f str = 
        File.AsyncWriteAllText(f,str)

    let writeLinesAsync (f:string) l = 
        File.AsyncWriteAllLines(f, l)
        
    ///readline without locking file
    let readLines (path:string) = seq {
        use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)            
        use sr = new StreamReader(fs)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
        }

    let writeFile f lines = 
        File.AsyncWriteAllLines(f, lines)

    ///move file with tests and return filename
    let moveFile outputdir (file:string) = 
      let f = FileInfo(file)
      try
        if outputdir <> f.DirectoryName then 
          let dstfile = outputdir +/ f.Name
          if File.Exists( dstfile ) then File.Delete ( dstfile )
          f.MoveTo(dstfile)
          dstfile
        else f.FullName
      with 
      | e -> failwithf "cannot move %s, %O" file e 

    let updatefileAsync (file:FileInfo) (destFile:FileInfo)  = 
        async{
          let destname =  destFile.FullName
          let backup = destname+"~"
          let rb () = 
              try 
                File.Delete (backup) 
              with
              |_ -> printfn "Cannot remove backup %s " backup
          do rb ()
          if destFile.Exists && file.LastWriteTime > destFile.LastWriteTime then 
              try 
                File.Move( destname, backup ) 
              with 
              |_ -> printfn "Cannot backup %s " destname
          do rb () 
          if not destFile.Exists then 
              printfn "updating %s" destFile.FullName
              do! copyToAsync file.FullName destFile.FullName
              destFile.LastWriteTimeUtc <- file.LastWriteTimeUtc
        }

    let updatedirAsync sourcePath destinationPath  = 
        //Create all of the directories
        for dirPath in Directory.GetDirectories(sourcePath, "*", SearchOption.AllDirectories) do
            Directory.CreateDirectory(dirPath.Replace(sourcePath, destinationPath)) |> ignore

        //Copy all the files & Replaces any files with the same name, except some files
        Directory.GetFiles(sourcePath, "*.*", SearchOption.AllDirectories)
        //|> Seq.filter( fun x -> not <| x.Contains(@"\csv\")  && not <| x.Contains("currentuser.txt"))
        |> Seq.filter( fun x -> not <| x.Contains("currentuser.txt"))
        |> Seq.map( fun newPath ->
            let file = FileInfo(newPath)
            let destFile = FileInfo( newPath.Replace(sourcePath, destinationPath) )
            updatefileAsync file destFile )
        |> Async.Parallel
        |> Async.Ignore
      
    ///read price from csv file into seq of string,float tuples
    let getPrice (f:string) = 
      use s = new FileStream(f, FileMode.Open, FileAccess.Read,
                        FileShare.ReadWrite);
      use fs = new StreamReader(s) 
      let tsk = fs.ReadToEndAsync()
      let price = CsvFile.Parse(tsk.Result)
      match price.NumberOfColumns with
        | 2 -> seq{
                for row in price.Rows do
                  //validate price is a number, skip empty pillar
                  match row.[0],row.[1] with
                  | "",_ -> ignore()
                  | p, Double v -> yield (p.ToUpper(), v)  
                  | p, v ->  failwithf "Wrong data format in file %s, pillar %s, value %s!" f p v
               } 
        | _ -> failwithf "Input data should have exactly 2 columns: %s" f 
               Seq.empty 

[<AutoOpen>]
module DateUtils = 
    open System.Text.RegularExpressions

    let generateDay (d:DateTime) (dir:bool) =
        let addday i = 
           match dir with
           | true -> d.AddDays i
           | false -> d.AddDays (-i)
        Seq.initInfinite float // 0.0 , 1.0, ...
        |> Seq.map addday 

    let generateMonth (d:DateTime) (dir:bool) = //not recurves and therefore it keeps roll date
        let nums =  Seq.initInfinite int // 0.0 , 1.0, ...
        match dir with
        | true -> Seq.map d.AddMonths nums 
        | false -> Seq.map ( (-) 0 >> d.AddMonths ) nums // equiv to: Seq.map( fun i -> d.AddMonths -i ) nums  
    
    let dateRange (startDate:DateTime) endDate =
         let n = abs((endDate - startDate).Days) + 1
         if startDate <= endDate then 
            Array.init n startDate.AddDays
         else
            Array.init n ( fun i -> startDate.AddDays(-i))

    let isHoliday (hol:Set<DateTime>) d = 
        hol.Contains d

    let isWeekend (d:DateTime) = 
        d.DayOfWeek = DayOfWeek.Saturday || d.DayOfWeek = DayOfWeek.Sunday 

    let isBusinessDay hol d =
        not ( isHoliday hol d  || isWeekend d ) 

    let generateBusinessDay hol (d:DateTime) (dir:bool) =
        generateDay d dir
        |> Seq.filter (isBusinessDay hol)

    let prevBusinessDay hol dt = generateBusinessDay hol dt false |> Seq.head
    let nextBusinessDay hol dt = generateBusinessDay hol dt true |> Seq.head

    /// add postive or negative number of bds. 0 bd means next bd.
    let addBusinessDay n hol (dt:DateTime) = 
        match n with
        | x when x > 0 -> generateBusinessDay hol (dt.AddDays 1.) true  |> Seq.skip (abs x - 1 ) |> Seq.head 
        | x when x < 0 -> generateBusinessDay hol (dt.AddDays -1.) false |> Seq.skip (abs x - 1) |> Seq.head
        | _ -> nextBusinessDay hol dt //1 case

    let modifiedFollowing hol dt = 
        let d0 = nextBusinessDay hol dt
        if d0.Month <> dt.Month then prevBusinessDay hol dt else d0

    let bdRange hol d1 d2 = 
        dateRange d1 d2 
        |> Array.filter( isBusinessDay hol)

    let numBizdays hol (d1:DateTime) d2 = //exclusive of d1
        if d1 > d2 then invalidArg "d1 d2" "d1 should not be greater than d2"
        if d1.Date <> d1 then invalidArg "d1" "d1 should not have time fractions"
        if d2.Date <> d2 then invalidArg "d1" "d2 should not have time fractions"
        if d1 = d2 then 0 else bdRange hol (d1.AddDays 1.0 ) d2 |> Array.length

    let getBizYears hol d1 d2 = 
        float ( numBizdays hol d1 d2 ) / 252.

    let getBizYears' d1 d2 = 
        getBizYears Set.empty d1 d2
    /// <summary>
    ///goes to following weekday, 
    ///starting monday at 1, sunday at 7 due to allow direction.
    ///3 for current or next wednesday, -3 for current or previous wednesday
    ///use -7 not -0 for previous sunday obviously
    /// </summary>
    /// <param name="d0"></param>
    /// <param name="n"></param>
    let dayOfWeek (d0:DateTime) n =    
        let t = if n >= 0 
                then ( -(int d0.DayOfWeek) + n + 7 ) % 7 
                else ( -( int d0.DayOfWeek ) - (n % 7) - 7 ) % 7 
        d0.AddDays(float t)

    let monthOfYear (d0:DateTime) n =    
        if abs(n) > 12 then invalidArg "n" " n should be between -12 and 12."
        match n with
        | n when abs(n) = d0.Month -> d0
        | n when n >= 0 -> ( n + 12 - d0.Month ) % 12 |> d0.AddMonths
        | _ ->  - ( d0.Month + n + 12 ) % 12 |> d0.AddMonths

    /// <summary>
    /// compute begining of current calendar period                        
    /// </summary>
    /// <param name="d0"></param>
    /// <param name="n">period number of month, 0 or 1 for month, 3 for quarter, 6 for half-year, 12 for year</param>
    let beginOfCalendarPeriod (d0:DateTime) (n:int) =    
        match n with
        | 1 | 0 -> DateTime(d0.Year,d0.Month,1)
        | 3 | 6 | 12 ->  
            let adj = (d0.Month - 1 ) % n  
            DateTime(d0.Year,d0.Month-adj,1)
        | _ -> invalidArg "n" "number of month should be in (1,3,6,12)"

    ///Take a holidays and initial date and adjust it using a date string.
    ///a date string is a string of actions case sensitive
    ///each token is composed of one or no +/- for direction
    ///then one or no number for repeats
    ///then one action character
    ///throw error if input string is not fully decomposible into tokens
    ///e.g. 2b-10d3Q+2ye or +3m+0F
    /// d: calendar days ignore holiday      
    /// w: calendar weeks ignore holiday
    /// m: calendar month ignore holiday
    /// y: calendar year ignore holiday
    /// b: move by business day 
    /// n: next bd
    /// p: previous bd
    /// f: modifiedFollowing, next bd unless cross month then previous
    /// a: beginning of month ignores repeat and direction    
    /// e: end of month ignores repeat and direction    
    /// A: begining of current calendar year 
    /// Z: end of current calendar year 
    /// Q: begining of current calendar quarter  
    /// H: begining of current calendar half year
    /// W: dayOfWeek 1 as Monday, .. 7 Sunday, 3W to go to wedneday, -7W to goes to previous Sunday
    /// C: caldenar monthOfYear 1 for Jan, -1 for prev Jan, keep days in month

    let dateAdjust (holidays:Set<DateTime>) (str:string) (d0:DateTime)= 
        let pattern = "([+-]?)(\d*)([a-zA-Z])"
        let patternfull = "^(([+-]?)(\d*)([a-zA-Z]))*$"
        if not (Regex.IsMatch (str,patternfull)) then failwithf "Invalid input: %s" str

        Regex.Matches(str, pattern) 
        |> Seq.cast 
        |> Seq.fold ( fun (d:DateTime) (m:Match) ->      
          let direction = 
            match Regex.Replace(m.Value, pattern, "$1") with 
            | "-" -> -1
            | _ -> 1
          let b,i = Int32.TryParse( Regex.Replace(m.Value, pattern, "$2"))
          let repeat = if b then i else 0
          match System.Text.RegularExpressions.Regex.Replace(m.Value, pattern, "$3") with       
          | "d" -> d.AddDays (float (direction * repeat)) //calendar days ignore holiday      
          | "w" ->  d.AddDays (float (direction * repeat * 7)) //calendar weeks ignore holiday
          | "m" -> d.AddMonths(direction * repeat) //calendar month ignore holiday
          | "y" -> d.AddYears(direction * repeat) //calendar year ignore holiday
          | "b" -> addBusinessDay (direction * repeat) holidays d //move by business day 
          //the followings are adjusts that ignores direction and repeat.                                 
          | "n" -> nextBusinessDay holidays d //next if not bd
          | "p" -> prevBusinessDay holidays d  //previous if non bd
          | "f" -> modifiedFollowing holidays d //next if not bd unless cross month then previous
          | "a" -> DateTime(d.Year,d.Month,1) //beginning of month ignores repeat and direction    
          | "e" -> DateTime(d.Year,d.Month,DateTime.DaysInMonth(d.Year,d.Month)) //end of month ignores repeat and direction    
          | "A" -> DateTime( d.Year, 1, 1)
          | "Z" -> DateTime(d.Year,12,31)//end of yr, and use A for beginning of yr                        
          | "Q" -> beginOfCalendarPeriod d 3
          | "H" -> beginOfCalendarPeriod d 6
          //number is used to define days of week instead of repeat, direction is kept
          | "W" -> dayOfWeek d (direction * repeat) //3W to go to wedneday, -7W to goes to previous Sunday
          | "C" -> monthOfYear d (direction * repeat) //1C to go to next Jan, -1C to goes to previous Jan
          | x -> failwith ("unknown str:" + x) ) d0

    let dateAdjust' = dateAdjust Set.empty //shortcut for no holiday checking, still check for weekends
    // create an active pattern to match time tenor
    let (|Tenor|_|) input =
       let tenors = ["ON"; "TN"; "SN" ; "SPOT"; "TODAY" ; "DAYAHEAD"; "WEEKEND" ] |> set
       if tenors.Contains input || input.StartsWith "BOM" then Some input else None

    let (|Period|_|) input =
       let m = Regex.Match(input,"^(\d+)(D|W|M|Y)$") 
       if (m.Success) then Some (int(m.Groups.[1].Value), m.Groups.[2].Value) else None  

    let (|FraPeriod|_|) input = //9m-12m etc
       let m = Regex.Match(input,"^(\d+)(D|W|M|Y)(\d+)(D|W|M|Y)$") 
       if (m.Success) then Some (int(m.Groups.[3].Value), m.Groups.[4].Value) else None  

    let pillarToDate (dStr:string) = 
      match datestr dStr with
      | YYYYMMDD d -> d
      | MMMYY d -> d
      | DDMMMYY d -> d
      | MMDDYY d -> d
      | Date d -> d
      | Period ( n, p ) | FraPeriod( n, p) -> 
          match p with
          |"D" -> DateTime.Today.AddDays( float n )
          |"W" -> DateTime.Today.AddDays( float n * 7.0 )
          |"M" -> DateTime.Today.AddMonths(n)
          |"Y" -> DateTime.Today.AddYears(n)
          | _  -> invalidOp "Unknown period type, expect D/W/M/Y"
      | Tenor d -> 
          match d with 
          |"ON" | "DAYAHEAD" -> DateTime.Today.AddDays( 1.0 )
          |"TN"|"SPOT" -> DateTime.Today.AddDays( 2.0 )
          |"SN" -> DateTime.Today.AddDays( 3.0 )
          |"TODAY" -> DateTime.Today
          |x when x.StartsWith "BOM" -> DateTime.Today |> dateAdjust' "e"
          | "WEEKEND" -> DateTime.Today |> dateAdjust' "7W"
          | _  -> invalidOp (sprintf "Unknown tenor string %s" d)
      | _ -> 
        //printfn "Warning: Unknown pillar %s, using today." dStr
        DateTime.Today
      
    /// <summary>
    /// allow broken period both end, d1 to month end, then each whole month, and finally month start to d2
    /// </summary>
    /// <param name="d1"></param>
    /// <param name="d2"></param>
    let generateCalMonthSchedule d1 d2 = 
        if d1 > d2 then failwith "d1 > d2, invalid inputs"
        generateMonth ( dateAdjust' "a" d1 ) true
        |> Seq.takeWhile ( fun d -> d <= d2 )
        |> Array.ofSeq
        |> Array.map( fun ms -> 
            let me =  dateAdjust' "e" ms             
            max ms d1, min me d2)
            
    let getPeriod (str:string) = //get period like Jan19, 1Q19, Cal19 etc
        let str = datestr str
        // create an active pattern to match time tenor
        let (|Quarter|_|) input =
            let m = Regex.Match(input,"^([1-9])Q(\d\d)$") //2Q19
            if (m.Success) then 
                Some (int (m.Groups.Item 1).Value , int (m.Groups.Item 2).Value)  
            else 
                None  
        let (|Year|_|) input =
            let m = Regex.Match(input,"^CAL(\d\d)$") //CAL19
            if (m.Success) then Some ( int (m.Groups.Item 1).Value )  else None  
        match str.ToUpper() with
        | MMMYY d -> d, (dateAdjust' "e" d)
        | Quarter (q,y) -> 
            let s = DateTime( y + 2000, 1 , 1) |> dateAdjust' ( sprintf "%im" ((q - 1 )* 3 ) )
            s, s |> dateAdjust' "2me"
        | Year y -> 
            let s = DateTime( y + 2000, 1 , 1) 
            s, s |> dateAdjust' "Z"
        | _ -> failwithf "Invalid period %s" str

    let formatPillar (x:DateTime) = x.ToString("MMM-yy").ToUpper() //e.g. DEC-20
