module Markdown

let insideTag tag content = sprintf "<%s>%s</%s>" tag content tag

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
    s |> stripHeaderMarkdown |> insideTag (sprintf "h%d" n)

let isSpanTag tag s = s |> startsWith tag && s |> endsWith tag

let isBold   = isSpanTag "__"
let isItalic = isSpanTag "_"

let stripSpanMarkdown (taglen:int) (s:string) =
    s.Substring(taglen, s.Length - (taglen * 2))

let stripBoldMarkdown   = stripSpanMarkdown 2
let stripItalicMarkdown = stripSpanMarkdown 1

let convertBold s =
    s |> stripBoldMarkdown |> insideTag "em"

let convertItalic s =
    s |> stripItalicMarkdown |> insideTag "i"

let isParagraph s = not (isBold s || isItalic s || isHeader s)

let stripParagraphMarkdown s = s

let convertParagraph s =
    s |> stripParagraphMarkdown |> insideTag "p"

let isListItem s = s |> startsWith "* " && s.Length > 2

let stripListItemMarkdown (s:string) = s.Substring(2)

let findSpanMarker (tag:string) (fromIdx:int) (s:string) =
    s.IndexOf(tag, fromIdx)

let hasSpanMarker tag s = findSpanMarker tag 0 s > -1

let findTaggedSpan tag s =
    let len = String.length tag
    if hasSpanMarker tag s then
        let startIdx = findSpanMarker tag 0 s
        let endIdx = findSpanMarker tag (startIdx + len) s
        if endIdx = -1 then None else Some (startIdx,endIdx)
    else
        None

let extractTaggedSpan tag s =
    let len = String.length tag
    match findTaggedSpan tag s with
    | None -> None
    | Some (startIdx,endIdx) ->
        Some (startIdx,endIdx,s.[startIdx + len .. endIdx - 1])

let substrBefore idx (s:string) =
    if idx <= 0 then "" else s.[..idx-1]

let substrAfter idx (s:string) =
    if idx >= s.Length then "" else s.[idx..]

let convertTaggedSpan tag htmlTag s =
    match extractTaggedSpan tag s with
    | None -> s
    | Some (startIdx,endIdx,content) ->
        sprintf "%s%s%s"
            (s |> substrBefore startIdx)
            (content |> insideTag htmlTag)
            (s |> substrAfter (endIdx + String.length tag))

let convertBoldSpan   = convertTaggedSpan "__" "em"
let convertItalicSpan = convertTaggedSpan "_"  "i"

let convertListItem s =
    let content = s |> stripListItemMarkdown
    let convertedContent =
        if isBold content then
            convertBold content
        elif isItalic content then
            convertItalic content
        else
            convertParagraph content
    convertedContent |> insideTag "li"

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
