module Markdown

let startsWith item (s:string) = s.StartsWith item
let endsWith item (s:string) = s.EndsWith item

let startCount ch s = s |> Seq.takeWhile ((=) ch) |> Seq.length

let isHeader (s:string) =
    let cnt = startCount '#' s
    1 <= cnt && cnt <= 6 && s.Length > cnt && s.[cnt] = ' '

let stripHeaderMarkdown (s:string) =
    let cnt = startCount '#' s
    s.Substring(cnt + 1)

let convertHeader s =
    let n = startCount '#' s
    let contents = stripHeaderMarkdown s
    sprintf "<h%d>%s</h%d>" n contents n

let isBold s   = s |> startsWith "__" && s |> endsWith "__"
let isItalic s = s |> startsWith "_"  && s |> endsWith "_"

let stripBoldMarkdown (s:string) = s.Substring(2, s.Length - 4)
let stripItalicMarkdown (s:string) = s.Substring(1, s.Length - 2)

let convertBold s =
    s |> stripBoldMarkdown |> sprintf "<em>%s</em>"

let convertItalic s =
    s |> stripItalicMarkdown |> sprintf "<i>%s</i>"

let isParagraph s = not (isBold s || isItalic s || isHeader s)

let stripParagraphMarkdown s = s

let convertParagraph s =
    s |> stripParagraphMarkdown |> sprintf "<p>%s</p>"

let isListItem s = s |> startsWith "* " && s.Length > 2

let stripListItemMarkdown (s:string) = s.Substring(2)

let findBoldMarker (fromIdx:int) (s:string) =
    s.IndexOf("__",fromIdx)
let findItalicMarker (fromIdx:int) (s:string) =
    s.IndexOf("_", fromIdx)

let hasBoldMarker   s = findBoldMarker   0 s > -1
let hasItalicMarker s = findItalicMarker 0 s > -1

let findBoldSpan s =
    if hasBoldMarker s then
        let startIdx = findBoldMarker 0 s
        let endIdx = findBoldMarker (startIdx+2) s
        if endIdx = -1 then None else Some (startIdx,endIdx)
    else
        None

let findItalicSpan s =
    if hasItalicMarker s then
        let startIdx = findItalicMarker 0 s
        let endIdx = findItalicMarker (startIdx+1) s
        if endIdx = -1 then None else Some (startIdx,endIdx)
    else
        None

let extractBoldSpan s =
    match findBoldSpan s with
    | None -> None
    | Some (startIdx,endIdx) ->
        Some (startIdx,endIdx,s.[startIdx+2 .. endIdx-1])

let extractItalicSpan s =
    match findItalicSpan s with
    | None -> None
    | Some (startIdx,endIdx) ->
        Some (startIdx,endIdx,s.[startIdx+1 .. endIdx-1])

let substrBefore idx (s:string) =
    if idx <= 0 then "" else s.[..idx-1]

let substrAfter idx (s:string) =
    if idx >= s.Length then "" else s.[idx..]

let convertBoldSpan s =
    match extractBoldSpan s with
    | None -> s
    | Some (startIdx,endIdx,content) ->
        sprintf "%s<em>%s</em>%s"
            (s |> substrBefore startIdx)
            content
            (s |> substrAfter (endIdx+2))

let convertItalicSpan s =
    match extractItalicSpan s with
    | None -> s
    | Some (startIdx,endIdx,content) ->
        sprintf "%s<i>%s</i>%s"
            (s |> substrBefore startIdx)
            content
            (s |> substrAfter (endIdx+1))

let convertListItem s =
    let content = s |> stripListItemMarkdown
    let convertedContent =
        if isBold content then
            convertBold content
        elif isItalic content then
            convertItalic content
        else
            convertParagraph content
    sprintf "<li>%s</li>" convertedContent

let rec parse (markdown: string) =
    let mutable html = ""
    let mutable isList = false

    let lines = markdown.Split('\n')

    for i = 0 to lines.Length - 1 do

        if isListItem lines.[i] then
            if not isList then
                html <- html + "<ul>"
                isList <- true
            html <- html + convertListItem lines.[i]

        elif isHeader lines.[i] then
            html <- html + convertHeader lines.[i]

        else
            let content =
                lines.[i]
                |> convertBoldSpan
                |> convertItalicSpan
            html <- html + convertParagraph content

    if isList then
        html <- html + "</ul>"

    html
