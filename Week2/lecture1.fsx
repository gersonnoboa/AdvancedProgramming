
(* Lecture 1 *)

// This is also a comment

/// This is also a comment

4/2

let x = 4/2

// finding duplicate words in  a string

// split a string into words at spaces

let splitAtSpaces (text: string) = text.Split ' ' |> Array.toList
let splitAtSpaces2 (text: string) = Array.toList (text.Split ' ' )


splitAtSpaces "hello world"

splitAtSpaces " "
splitAtSpaces ""

splitAtSpaces "hello  world"

let wordCount text = 
    let words = text |> splitAtSpaces
    let numWords = words.Length
    let wordSet = Set.ofList words
    let numDups = numWords - wordSet.Count in (numWords, numDups)

let wordCount2 text = 
    let words = text |> splitAtSpaces
    let numWords = words.Length
    let wordSet = Set.ofList words
    let numDups = numWords - wordSet.Count 
    (numWords, numDups)

wordCount "hello world"
wordCount "hello world world"

let showWordCount text = 
  let numWords, numDups = wordCount text
  printfn "--> %d words in text" numWords
  printfn "--> %d duplicate words" numDups

showWordCount "All the king's horses and all the king's men"

let powerOfFour n = (n*n)*(n*n)

powerOfFour 2

let powerOfFour' n = (n*n)*(n*n)*1.0

let badDefinition1 =
 let words = splitAtSpaces text
 let text = "we three kings"
 words.Length


let badDefinition2 = badDefinition2 +1

let powerOfFourPlusTwo n = 
    let n1 = n * n
    let n2 = n1 * n1
    let n3 = n2 + 2
    n3

powerOfFourPlusTwo 2


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