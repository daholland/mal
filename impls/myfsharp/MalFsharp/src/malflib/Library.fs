module Malf.Malf

// type MalfAtom = MalfString of string
//               | MalfNumber of float
//               | MalfBool of bool
//               | MalfNil

// type MalfCollection = MalfList
//                     | MalfVector
//                     | MalfHashMap


type MalfType = MalfString of string
              | MalfNumber of float
              | MalfBool of bool
              | MalfNil
              | MalfSymbol of string
              | MalfKeyword of string
              | MalfList of MalfType list
              | MalfVector of MalfType array
              | MalfHashMap of Map<string,MalfType>



[<RequireQualifiedAccess>]
module Reader =
    open FParsec
    type Parser<'t> = Parser<'t, MalfType>
    let test p str =
        printfn "==== fpparsec ===="
        match run p str with
        | Success(result, _, _) -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %A result" errorMsg
        printfn "====/fpparsec ===="
        MalfString str

    let _pfloat = pfloat

    let next = ()
    let peek = ()

    let read_str = ()

    let tokenize = ()

    let read_form = ()

    let read_list = ()

    let read_atom = ()

    let malfnil = stringReturn "nil" MalfNil
    let malftrue = stringReturn "true" (MalfBool true)
    let malffalse = stringReturn "false" (MalfBool false)

    let malfnumber = pfloat |>> MalfNumber

    let str s = pstring s

    let stringLiteral =
        let escape = anyOf "\"\\/bfnrt"
                     |>> function
                         | 'b' -> "\b"
                         | 'f' -> "\u000C"
                         | 'n' -> "\n"
                         | 'r' -> "\r"
                         | 't' -> "\t"
                         | c -> string c

        let unicodeEscape =
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (str "\"") (str "\"")
                (stringsSepBy normalCharSnippet escapedCharSnippet)

    let malfstring = stringLiteral |>> MalfString

    let malfval, malfvalref = createParserForwardedToRef()

    let ws = spaces
    let malfsym =
        let isSymbolFirstCharacter c = isLetter c || c = '_' || c = '+'
        let isSymbolChar c = isLetter c || c = '-' || c = '!' || c = '?' || c = '*' || c = '_'

        many1Satisfy2L isSymbolFirstCharacter isSymbolChar "symbol" |>> MalfSymbol

    let malfcall = malfsym .>>. (ws >>. malfval)

    let testasdf = MalfList [MalfSymbol "asdf"; MalfNumber 1.0; MalfList [MalfNil]]

// MalfList (MalfSymbol MalfNumber MalfList(MalfSym MalfNumber MalfNumber


    let listBetweenStrings sOpen sClose pElement f =
        between (str sOpen) (ws >>. str sClose)
                (ws >>. (sepEndBy (pElement) (str " " .>> ws )) .>> ws |>> f)

    let malflist = (listBetweenStrings "(" ")" malfval MalfList) //.>> ws

    do malfvalref := choice [malfstring
                             malfnumber
                             malftrue
                             malffalse
                             malfnil
                             malfsym
                             malflist]

    let malf = ws >>. malfval .>> ws .>> eof

    let parseMalfString str = match run malf str with
                              | Success(result, _, _) -> result
                              | Failure(errMsg, _, _) -> MalfString errMsg



let Read_d a = Reader.test Reader.malf a
let Read a = Reader.parseMalfString a

let Eval a = a

module Printer =
    let pr_str = ()

    let rec printlist l =
        let inner = List.map printmalftype l
        printfn "asdfasdf"
        for foo in inner do printfn "%s" foo
        "(" + (String.concat " " (Seq.ofList inner)) + ")"
    and printmalftype m =
        match m with
        | MalfString inner -> "\"" + inner + "\""
        | MalfNumber inner -> string inner
        | MalfBool inner -> string inner
        | MalfNil -> "nil"
        | MalfSymbol inner -> inner
        | MalfKeyword inner -> inner
        | MalfList inner -> printlist inner
        | MalfVector inner -> string inner
        | MalfHashMap inner -> string inner

let Print a = Printer.printmalftype a //printmaltype instead??

let Test a =
    printfn "tet!"
    a

let REP a = a |> Read |> Eval |> Test |> Print
