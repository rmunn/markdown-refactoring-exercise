module Markdown

let startsWith (s:string) = s.StartsWith
let endsWith (s:string) = s.EndsWith

let startCount ch s = s |> Seq.takeWhile ((=) ch) |> Seq.length

let isHeader (s:string) =
    let cnt = startCount '#' s
    1 <= cnt && cnt <= 6 && s.Length > cnt && s.[cnt] = ' '

let stripHeaderMarkDown (s:string) =
    let cnt = startCount '#' s
    s.Substring(cnt + 1)

let parseHeader s =
    let n = startCount '#' s
    let contents = stripHeaderMarkDown s
    sprintf "<h%d>%s</h%d>" n contents n

let rec parse (markdown: string) =
    let mutable html = ""
    let mutable remainder = markdown
    let mutable isList = false

    let lines = remainder.Split('\n')

    for i = 0 to lines.Length - 1 do

        if lines.[i].[0] = '*' then
            if not isList then
                html <- html + "<ul>"
                isList <- true

            html <- html + "<li>"

            let mutable line = lines.[i].[2..]
            let mutable __pos = line.IndexOf "__"

            let notusep = line.StartsWith "__" || line.StartsWith "_"

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

            if not notusep then
                html <- html + "<p>"

            html <- html + line

            if not notusep then
                html <- html + "</p>"

            html <- html + "</li>"

        elif isHeader lines.[i] then
            html <- html + parseHeader lines.[i]
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
