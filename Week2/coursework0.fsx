(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 0: Getting started

  ------------------------------------
  Name: Luis Noboa
  Student ID: lunobo
  ------------------------------------

  Answer the questions below.  You answers to questions 2--8 should be
  correct F# code written after the question. The F# code for question
  1 is written for you and serves as an example. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will NOT be graded but we encourage you to do it,
  you will not succeed in this course if you don't practice, and
  there's no time like the present! Also, you may find that parts of
  it appear in later courseworks.

*)

// 0. Find your way to the fsharp interactive (fsi) command prompt.
//    I.e. log in to a lab machine and start Visual Studio, install
//    VS/Mono on your laptop, etc.

// 1. Load the following function into fsi

let greeting name = printfn "Hello: %s" name 

// 2. Run the function 'greeting' and say hello to yourself.
//    Type the expression below.
greeting "Gerson"

// 3. Define
//    'splitAtChar : text:string -> sep:char -> list<string>'
let splitAtChar (text:string) (sep:char) = text.Split sep |> Array.toList
splitAtChar "hola" 'l'

// 4. Modify splitAtSpaces to use splitAtChar
let splitAtSpaces (text: string) = splitAtChar text ' '
splitAtSpaces "barcelona campeon"
// 5. Define 'sentenceCount : text:string -> int'
let sentenceCount text = 
    let sentences = splitAtChar text '.'
    sentences.Length
    
sentenceCount "Hola. Chao. Un solo idolo tiene el Ecuador. Barcelona campeon"
// 6. Define 'stats : text:string -> unit'
//    which prints the same stats as showWordCount +
//    the number of sentences and average length of sentences
//    hint: try float: 'int -> float'
let wordCount text = 
    let words = text |> splitAtSpaces
    let numWords = words.Length
    let wordSet = Set.ofList words
    let numDups = numWords - wordSet.Count in (numWords, numDups)

let showWordCount text = 
  let numWords, numDups = wordCount text
  printfn "--> %d words in text" numWords
  printfn "--> %d duplicate words" numDups

let showSentenceCount text =
    printfn "--> %d sentences in text" (sentenceCount text)
    
let stats text = 
    showWordCount text
    showSentenceCount text
    printfn "--> %f average length of sentences" (float(fst (wordCount text)) / float(sentenceCount text))

stats "Bla blab bla bla bla. Bbla bla bla bla. Bla bla bla bla bla bla bla bla bla bla"

// 7. Use the 'http' function from the lecture to download the file
//    http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
//    NOTE: you cannot use this function in tryfsharp. Instead you can
//    paste the text into your file as a string and process it locally

// http function required for coursework:

open System.IO
open System.Net

// get the contents of the url via a web request
let http (url: string) =
  let req = WebRequest.Create(url)
  let resp = req.GetResponse()
  let stream = resp.GetResponseStream()
  let reader = new StreamReader(stream)
  let html = reader.ReadToEnd()
  resp.Close()
  html

http "http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt"
// 8. run 'stats' on the downloaded file

stats (http "http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt")