module Markdown

// "Extension functions" are possible for standard modules, too
module List =
    /// Divide a list up into chunks by the result of a key fn.
    /// Each chunk consists of items for which the key function
    /// returned the same value. Once the key function returns a
    /// new value (different from the previous item), a new chunk
    /// will be started. But if the key function later returns
    /// a previous value, it will start a *new* chunk, not add to
    /// the "first" chunk.
    /// I.e., calling `chunkBy isOdd [1; 3; 2; 4; 6; 5; 7]`
    ///       returns `[[1; 3]; [2; 4; 6]; [5; 7]]`.
    let chunkBy keyFun list =
        if List.isEmpty list then [] else
        let rec inner acc currKey list =
          [ match list with
            | [] -> yield (List.rev acc)
            | x::xs ->
                let key = keyFun x
                if key = currKey then
                    yield! inner (x::acc) currKey xs
                else
                    yield (List.rev acc)
                    yield! inner [x] key xs ]
        inner [] (List.head list |> keyFun) list

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

let stripParagraphMarkdown s = s

let convertParagraph s =
    s |> stripParagraphMarkdown |> insideTag "p"

let isListItem s = s |> startsWith "* " && s.Length > 2

let stripListItemMarkdown (s:string) = s.Substring(2)

let findSpanMarker (tag:string) (fromIdx:int) (s:string) =
    s.IndexOf(tag, fromIdx)

let hasSpanMarker tag s = findSpanMarker tag 0 s > -1

let hasBold   = hasSpanMarker "__"
let hasItalic = hasSpanMarker "_"

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
        if hasBold content then
            convertBoldSpan content
        elif hasItalic content then
            convertItalicSpan content
        else
            convertParagraph content
    convertedContent |> insideTag "li"

let convertLine line =
    if isListItem line then
        convertListItem line

    elif isHeader line then
        convertHeader line

    else
        line
        |> convertBoldSpan
        |> convertItalicSpan
        |> convertParagraph

let isHtmlListItem htmlLine =
    htmlLine |> startsWith "<li>" && htmlLine |> endsWith "</li>"

// Given a chunked list of HTML segments, find the list items
// and wrap them with "<ul>" and "</ul>".
let wrapHtmlListWithUL chunk =
    if chunk |> List.head |> isHtmlListItem then
        "<ul>" :: chunk @ ["</ul>"]
    else
        chunk

let parse (markdown: string) =
    markdown.Split('\n')
    |> List.ofArray
    |> List.map convertLine
    |> List.chunkBy isHtmlListItem
    |> List.collect wrapHtmlListWithUL
    |> String.concat ""
