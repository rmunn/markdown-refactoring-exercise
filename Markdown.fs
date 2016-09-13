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

// AST for Markdown text

let outOfRange name value msg =
    new System.ArgumentOutOfRangeException(name, value, msg)
    |> raise

type Int6 = Int6 of int
let mkInt6 i =
    if 1 <= i && i <= 6 then
        Int6 i
    else
        outOfRange "i" i "Must be between 1 and 6"

type InlineNodes = InlineNode list
and InlineNode =
    | ItalicText of InlineNodes
    | BoldText of InlineNodes
    | PlainText of string

type WholeLineNode =
    | HeaderNode of Int6 * InlineNodes
    | ListStartMarker
    | ListItemNode of InlineNodes
    | ListEndMarker
    | ParagraphNode of InlineNodes

// Active patterns for Markdown matching

let (|Header|_|) (s:string) =
    let cnt = startCount '#' s
    if 1 <= cnt && cnt <= 6 && s.Length > cnt && s.[cnt] = ' '
        then Some (mkInt6 cnt,s.[cnt+1..])
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

// Parsing Markdown into AST

let rec parseText text =
    match text with
    | "" -> []
    | Bold (before,span,after) ->
        parseTaggedSpan BoldText (before,span,after)
    | Italic (before,span,after) ->
        parseTaggedSpan ItalicText (before,span,after)
    | _ -> [PlainText text]

and parseTaggedSpan nodeConstructor (before,span,after) =
    parseText before @
    (parseText span |> nodeConstructor) ::
    parseText after

let parseLine line =
    match line with
    | ListItem content -> ListItemNode (parseText content)
    | Header (lvl,content) -> HeaderNode (lvl,parseText content)
    | text -> ParagraphNode (parseText text)

// Output AST as HTML

let rec htmlFromInlineNode = function
    | PlainText  text     -> text
    | BoldText   contents -> contents |> htmlFromInlineNodes "em"
    | ItalicText contents -> contents |> htmlFromInlineNodes "i"

and htmlFromInlineNodes tag nodes =
    nodes
    |> List.map htmlFromInlineNode
    |> String.concat ""
    |> insideTag tag

let htmlFromNode = function
    | ParagraphNode content ->
        content |> htmlFromInlineNodes "p"
    | HeaderNode (Int6 level,content) ->
        let tag = sprintf "h%d" level
        content |> htmlFromInlineNodes tag
    | ListItemNode content ->
        let wrapInP = match content with
                      | [BoldText _]
                      | [ItalicText _] -> false
                      | _ -> true
        if wrapInP then
            content |> htmlFromInlineNodes "p" |> insideTag "li"
        else
            content |> htmlFromInlineNodes "li"
    | ListStartMarker -> "<ul>"
    | ListEndMarker  -> "</ul>"

let isListItem = function ListItemNode _ -> true | _ -> false

let wrapListWithMarkers chunk =
    if chunk |> List.head |> isListItem then
        ListStartMarker :: chunk @ [ListEndMarker]
    else
        chunk

let parse (markdown: string) =
    markdown.Split('\n')
    |> List.ofArray
    |> List.map parseLine
    |> List.chunkBy isListItem
    |> List.map wrapListWithMarkers
    |> List.collect (List.map htmlFromNode)
    |> String.concat ""
