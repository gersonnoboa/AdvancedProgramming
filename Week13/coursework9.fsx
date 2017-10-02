(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 9: Asynchronous and reactive programming

  ------------------------------------------------------------------------------
  Name: Luis Noboa
  Student ID: lunobo
  ------------------------------------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework9.fsx in directory coursework9.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, December 9, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function downloadParallel : string list -> Async<string []> that takes
  a list of URLs and downloads the resources referenced by these URLs in
  parallel. Use the function downloadAsync from the lecture in your
  implementation.
*)

open System
open System.IO
open System.Net

let readToEndAsync (reader: StreamReader) =
    Async.AwaitTask(reader.ReadToEndAsync())

let downloadAsync(url: string) =
    async{
        let uri = new Uri(url)
        let request = HttpWebRequest.Create(uri)
        use! response = request.AsyncGetResponse()
        let stream = response.GetResponseStream()
        use reader = new StreamReader(stream)
        return! readToEndAsync reader
    }

let rec getDownloads(list: string list) =
    match list with
        | [] -> []
        | h::t -> downloadAsync h :: getDownloads t

let rec downloadParallel (list: string list) =
    let downloads = getDownloads list
    Async.Parallel downloads

let downloads = downloadParallel(["http://tut.ee";"http://ut.ee"])
Async.RunSynchronously downloads
    
(*
  Task 2:

  Write a function downloadSemiParallel : string list -> Async<string []> that
  takes a list of URLs and downloads the resources referenced by these URLs.
  Resources from URLs with the same domain name shall be downloaded
  sequentially, but otherwise, parallelism shall be used. The order of the
  resources in the resulting array can be chosen by you.
*)

let downloadSync (url : string) =
  let request  = HttpWebRequest.Create(url)
  use response = request.GetResponse()
  let stream   = response.GetResponseStream()
  use reader   = new StreamReader(stream)
  reader.ReadToEnd()


let urls = (["http://tut.ee";"http://tut.ee/admissions"; "http://ut.ee"])

let rec getDownloadsSemiParallel (list: string list) =
    match list with
    | [] -> []
    | h::t -> new Uri(h) :: getDownloadsSemiParallel t

let group (list: string list) = List.ofSeq(Seq.ofList (getDownloadsSemiParallel list) |> Seq.groupBy (fun x -> x.Host))

let rec convertEverythingToList (group: (string * seq<Uri>) list) =
    match group with
    | [] -> []
    | (host,uris) :: t -> ((List.ofSeq uris) |> List.map (fun x -> x.AbsoluteUri)) :: convertEverythingToList t

let downloadSequentially list = 
    async{
        let all = list |> List.map(fun x -> downloadSync x)
        return String.concat ("") (Seq.ofList all)
    }

let rec downloadAsynchrously list = 
    match list with
    | [] -> []
    | h::t -> downloadSequentially h :: downloadAsynchrously t

let downloadSemiParallel urls = Async.Parallel (downloadAsynchrously (convertEverythingToList(group urls)))

let prueba = Async.RunSynchronously (downloadSemiParallel urls)
(*
  Task 3:

  Write an event stream additions : IObservable<string> that emits an event
  everytime a file is created in the current directory. Each such event shall
  carry the name of the created file.

  Furthermore, write an event stream removals : IObservable<string> that emits
  an event everytime a file is removed from the current directory. Each such
  event shall carry the name of the removed file.
*)

open System.IO
let watcher = new FileSystemWatcher(__SOURCE_DIRECTORY__)
watcher.EnableRaisingEvents <- true

let creatings = watcher.Created
let additions = creatings |> Observable.map(fun eventArgs ->
    sprintf "%s added" eventArgs.Name
    )

let removings = watcher.Deleted
let removals = removings |> Observable.map(fun eventArgs ->
    sprintf "%s removed" eventArgs.Name
    )

let printAdditions = additions |> Observable.add (printfn "%s")
printAdditions

let printRemovals = removals |> Observable.add (printfn "%s")
printRemovals

(*
  Task 4:

  Below you find the definition of a type Change whose values represent changes
  to a directory. Use the event streams from Task 3 to define an event stream
  changes : IObservable<Change> of all file additions and removals in the
  current directory.
*)

type Change =
  | Addition of string
  | Removal  of string

let wrapAdditions = creatings |> Observable.map (fun eventArgs ->
    Addition eventArgs.Name
    )
let wrapRemovals = removings |> Observable.map (fun eventArgs ->
    Removal  eventArgs.Name
    )

let changes = Observable.merge wrapAdditions wrapRemovals

let printChanges = changes |> Observable.add (printfn "%O")
printChanges

(*
  Task 5:

  Use the event stream changes from Task 4 to define an event stream
  turnover : IObservable<int> that tells at every file addition or removal how
  much the number of files in this directory has increased since the beginning
  (with negative numbers signifying a decrease). For example, if two files are
  added and one file is removed afterwards, there should be three events, that
  carry the numbers 1, 2, and 1, respectively.
*)

let turnover = 
    changes |> Observable.scan (fun count evt -> 
    match evt with
    | Addition _ -> count + 1
    | Removal _ -> count - 1
    ) 0

let printTurnover = turnover |> Observable.add (printfn "%d")

printTurnover