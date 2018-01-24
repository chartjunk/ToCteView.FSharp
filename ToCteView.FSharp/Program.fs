open System
open System.Windows.Forms

let newline = Environment.NewLine
let tab = "\t"

let split (splitter:string) (value:string) = value.Split([|splitter|], StringSplitOptions.None) |> List.ofArray
let splitRow = 
    split tab 
    >> List.map (function
    | x when x = "NULL" -> x
    | x -> sprintf "N'%s'" (x.Replace("'", "''")))

let rec rowsToSelects = 
    function
    | x::_, _ when x = "" -> ""
    | x::xs, h -> 
        (List.fold2 (sprintf "%s%s AS [%s], ") (newline + "UNION ALL SELECT ") (splitRow x) h).TrimEnd(' ', ',') + 
        rowsToSelects (xs,h)
    | _ -> ""

let toCteView = 
    split newline 
    >> fun l -> [";WITH mycte AS ( "] @ [rowsToSelects(l.Tail, split tab l.Head).[12..]] @ [") SELECT * FROM mycte"]
    >> String.concat newline

[<EntryPoint; STAThread>]
let main argv =
    Clipboard.GetText(TextDataFormat.UnicodeText) |> toCteView |> Clipboard.SetText
    0
