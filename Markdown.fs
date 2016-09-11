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
    let mutable remainder = markdown
    let mutable isList = false

    let lines = remainder.Split('\n')

    for i = 0 to lines.Length - 1 do

        if isListItem lines.[i] then
            if not isList then
                html <- html + "<ul>"
                isList <- true
            html <- html + convertListItem lines.[i]

        elif isHeader lines.[i] then
            html <- html + convertHeader lines.[i]

        else
            let mutable line = lines.[i]
            let mutable __pos = line.IndexOf "__"

            while __pos > -1 do
                let mutable __pos' = if __pos >= (line.Length - 2) then -1 else line.IndexOf("__", __pos + 2)

                if __pos' > -1 then
                    if __pos + 2 >= (line.Length - 1) then
                        line <- line.[0.. __pos - 1] + "<em>" + line.[__pos + 2 .. __pos' - 1] + "</em>"
                        __pos <- __pos' + 1
                    else
                        line <- line.[0.. __pos - 1] + "<em>" + line.[__pos + 2 .. __pos' - 1] + "</em>" + line.[__pos' + 2 ..]
                        __pos <- __pos' + 1
                else
                    __pos <- -1

            __pos <- line.IndexOf "_"

            while __pos > -1 do
                let mutable __pos' = if __pos >= (line.Length - 1) then -1 else line.IndexOf("_", __pos + 1)

                if __pos' > -1 then
                    if __pos + 1 >= (line.Length - 1) then
                        line <- line.[0.. __pos - 1] + "<i>" + line.[__pos + 1 .. __pos' - 1] + "</i>"
                        __pos <- __pos' + 1
                    else
                        line <- line.[0.. __pos - 1] + "<i>" + line.[__pos + 1 .. __pos' - 1] + "</i>" + line.[__pos' + 1 ..]
                        __pos <- __pos' + 1
                else
                    __pos <- -1

            html <- html + "<p>" + line + "</p>"

    if isList then
        html <- html + "</ul>"

    html
