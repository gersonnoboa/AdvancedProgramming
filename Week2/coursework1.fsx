(*

  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 1: Basic operations on lists

  ------------------------------------
  Name: Luis Noboa
  TUT Student ID: lunobo
  ------------------------------------


  Answer the questions below.  You answers to questions 1--9 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded.

  To submit the coursework you will be asked to
  
  1) Check out your empty GIT repository
  from the server git.ttu.ee using instructions on page
  https://courses.cs.ttu.ee/pages/ITT8060

  2) Put your solution into a file coursework1/coursework1.fsx
  in the repository. Commit it and push it to the server!

  NB! It is very important to make sure you use the exact name using
  only small caps. Files submitted under wrong name may not get a grade.

 
*)

// 1. Make an empty list of generic type.
let emptyList = []

// 2. Make an empty list of type 'char list' (or list<char>).
let emptyCharList : char list = []

// 3. Make a three element list called 'unis' containing pairs of university
// website url (string) and year of establishment (int). The year of
// establishement should be that of the university.
let unis = ["http://www.ut.ee", 1632 ; "http://www.uees.edu.ec", 1993 ; "http://www.ttu.ee", 1918]

// 4. Write a function filterOutYoungerThan: int -> string * int list -> string * int list to filter out universities
// which are less than some integer years old.  It should use the List.filter
// function from the library.

let filterOutYoungerThan (yearsOld: int) (unis: (string * int) list) = 
    unis |> List.filter (fun (web, est) -> (System.DateTime.Now.Year - est >= yearsOld))

// 5. Test the function 'filterOutYoungerThan' to filter out universities younger than 100 years in 
// your list 'unis'.
filterOutYoungerThan 100 unis

// 6. Calculuate the average age of your list of universities. The
// function should use pattern matching and recursion.

let average unis =
    let rec total tUnis =
        match tUnis with
        | [] -> 0
        | head::tail -> (System.DateTime.Now.Year - snd head) + total tail
    float(total unis) / float(unis.Length)

average unis
// 7. Using the http function write a function
//
//    getSource : (string * int) -> (string * string)
//
//    which takes a pair of a url and a year of establishment of the university and
//    returns a pair of a url and the html source of the page.

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

let getSource (uniTuple: (string * int)) =
    let uniURL = fst uniTuple
    uniURL, http uniURL

getSource unis.[0]

// 8. Write a function
//
//    getSize : (string * string) -> (string * int)
//    
//    which takes a pair of a url and its html source and returns a
//    pair of the url and the size of the html (length of the string).

let getSize (htmlTuple: string * string) =
    fst htmlTuple, (snd htmlTuple).Length

getSize (getSource unis.[0])
// 9. Write a function
//
//    getSourceSizes : (string * int) list -> (string * int) list
//
//    It should take a list of pairs of urls and yearselca  of
//    establishment and return a list of pairs of urls and
//    corresponding html source sizes

let rec getSourceSizes (unis: (string * int) list) =
    unis |> List.map(fun (url, year) -> getSize(getSource (url, year)))

getSourceSizes(unis)