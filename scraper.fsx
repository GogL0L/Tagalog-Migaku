//OBSERVE THAT PERMISSION FROM PINOY DICTIONARY © MUST BE REQUIRED IN ORDER TO
//EXECUTE THE CODE AND THUS SCRAPE THE WEBSITE. THE PURPOSE OF THIS CODE IS
//EXCLUSIVELY FOR EDUCATIONAL REASONS, SPECIFICALLY HOW ONE WOULD SCRAPE WITH A
//FUNCTIONAL LANGUAGE, IN THIS CASE FSHARP, APART OF THE DOTNET FRAMEWORK BY
//MICROSOFT ©.


#r "nuget: FSharp.Data, 4.0.1"
#r "nuget: FsVerbalExpressions, 0.6.1"
#r "nuget: FSharp.Json, 0.4.0"

open FSharp.Data
open FsVerbalExpressions.FsRegEx
open System
open System.IO
open FSharp.Json

printfn "Header works"


type Entry = {Word: string
              Definition: string
              Conjugations: list<string>}


let firstChildText selector (post : HtmlNode) =
    post.CssSelect(selector).[0].DirectInnerText().Trim()


let wordGroupToWord (wordGroup : HtmlNode) : string =
    wordGroup.CssSelect(".word-entry").[0]
    |> firstChildText "a"

let wordGroupToInfo (wordGroup : HtmlNode) : string =
    wordGroup.CssSelect(".definition").[0]
    |> firstChildText "p"


let infoToConjugations (info:string) : List<string> =
    let prefix = @"(.+?)\."
    let parenthesis = @"\((?<conjugations>.*)\)"
    info
    |> matches prefix
    |> Array.head
    |> sprintf "%A"
    |> capture parenthesis "conjugations"
    |> sprintf "%s"
    |> fun str -> str.Split [|','|]
    |> Seq.map (fun str -> str.Trim ())
    |> Seq.toList
    |> fun lst -> if lst = [""] then [] else lst
    //|> List.map String.Trim


let infoToDefinitions (info:string) : string =
    let suffix = @"\w*\.(.+)"
    info
    |> matches suffix
    |> Array.head
    |> sprintf "%A"


let wordGroupToConj (wordgroup : HtmlNode) : List<string> =
    wordgroup
    |> wordGroupToInfo
    |> infoToConjugations
    

let wordGroupToDef (wordgroup : HtmlNode) : string =
    wordgroup
    |> wordGroupToInfo
    |> infoToDefinitions
    
let wordGroupToEntry (wordgroup : HtmlNode) : Entry =
    {Word = wordGroupToWord wordgroup
     Definition = wordGroupToDef wordgroup
     Conjugations = wordGroupToConj wordgroup}


let getWebPageURL (char:char) (number:int) : string = 
    sprintf "%s/%O/%s/"
        "https://tagalog.pinoydictionary.com/list"
        char
        (string number)


type MigakuDictEntry = {
    term : string
    altterm : string
    pronunciation : string
    definition : string
    pos : string
    examples: string
    audio : string
    }

type MigakuConjEntry = {
    inflected : string
    dict : List<string>
    }

let createMigakuDictEntry (word:string) (def:string) : MigakuDictEntry = {
    term = word
    altterm = ""
    pronunciation = ""
    definition = def
    pos = ""
    examples = ""
    audio = ""
    }

let createMigakuConjEntry
    (word:string)
    (conjugations:List<string>)
    : List<MigakuConjEntry> =
    if conjugations = [] then [] else
        conjugations |> List.map (fun conj -> {inflected = conj; dict = [word]})


let scrapeSite : string ->  List<MigakuDictEntry> * List<MigakuConjEntry> =
    fun url->
        let doc = HtmlDocument.Load(url)
        let words = doc.CssSelect(".word-group")
        let dicList =
            words
            |> List.map wordGroupToEntry
            |> List.map (fun entry ->
                         createMigakuDictEntry entry.Word entry.Definition)
        let conjList =
            words
            |> List.map wordGroupToEntry
            |> List.map (fun entry ->
                         createMigakuConjEntry entry.Word entry.Conjugations)
            |> Seq.concat
            |> List.ofSeq
        (dicList, conjList)


let scrape : unit -> unit =
    fun () ->
        let mutable dic = []
        let mutable conj = []
        //for letterID in ['a' .. 'z'] do
        for charID in ['a' .. 'z'] do
            let mutable intID = 1
            let mutable loop = true
            while loop do
                let URL = getWebPageURL charID intID
                printfn "Scraping: %s" URL
                let request = Http.Request(URL, silentHttpErrors = true)
                let status = request.StatusCode

                if status = 404 then do
                    loop <- false
                else do
                    let dicList, conjList = scrapeSite URL
                    dic <- dic @ dicList
                    conj <- conj @ conjList
                    intID <- intID + 1
        printfn "Writing.."
        let dic_json = Json.serialize dic
        let conj_json = Json.serialize conj
        File.WriteAllText ("dictionary.json", dic_json)
        File.WriteAllText ("conjugations.json", conj_json)
        printfn "Finnished!"


scrape ()
