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

// String-related functions

let insideTag tag content = sprintf "<%s>%s</%s>" tag content tag

let startsWith item (s:string) = s.StartsWith item
let endsWith item (s:string) = s.EndsWith item

let substrBefore idx (s:string) =
    if idx <= 0 then "" else s.[..idx-1]

let substrAfter idx (s:string) =
    if idx >= s.Length then "" else s.[idx..]

let startCount ch s = s |> Seq.takeWhile ((=) ch) |> Seq.length

// Active patterns for Markdown matching

let (|Header|_|) (s:string) =
    let cnt = startCount '#' s
    if 1 <= cnt && cnt <= 6 && s.Length > cnt && s.[cnt] = ' '
        then Some (s.[cnt+1..],cnt)
        else None

let (|ListItem|_|) s =
    if s |> startsWith "* " && s.Length > 2
        then Some s.[2..]
        else None

let (|TaggedSpan|_|) (tag:string) (s:string) =
    if s.IndexOf(tag) = -1 then
        None
    else
        let len = String.length tag
        let startIdx = s.IndexOf(tag)
        let endIdx = s.IndexOf(tag, startIdx + len)
        if endIdx = -1 then
            None
        else
            Some (s |> substrBefore startIdx,
                  s.[startIdx + len .. endIdx - 1],
                  s |> substrAfter (endIdx + len))

let (|Bold|_|)   = (|TaggedSpan|_|) "__"
let (|Italic|_|) = (|TaggedSpan|_|) "_"

// Converting Markdown to HTML

let rec convertLinePart part =
    match part with
    | Bold (before,bolded,after) ->
        convertTaggedSpan "em" (before,bolded,after)
    | Italic (before,italicized,after) ->
        convertTaggedSpan "i" (before,italicized,after)
    | text -> text

and convertTaggedSpan htmlTag (before,span,after) =
  [ convertLinePart before
    span |> insideTag htmlTag
    convertLinePart after ] |> String.concat ""

let convertListItem item =
    let line = convertLinePart item
    let line' =
        if line |> startsWith "<" && line |> endsWith ">" then
            line
        else
            line |> insideTag "p"
    line' |> insideTag "li"

let convertLine line =
    match line with
    | ListItem item -> convertListItem item
    | Header (content,num) ->
        let tag = sprintf "h%d" num
        content |> insideTag tag
    | text -> text |> convertLinePart |> insideTag "p"

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
