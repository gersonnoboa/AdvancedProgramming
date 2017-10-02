(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists, recursion

  ------------------------------------
  Name: Luis Noboa
  TUT Student ID: lunobo
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by September 30, 2016.
*)

// 1. Make a value sl containing empty list of type string list.
let sl : string list = []

// 2. Make a function shuffle: int list -> int list that rearranges the elements of the argument list
// in such a way that the first value goes to first, last to second,
// second to third, last but one to fourth etc.
// E.g.
// shuffle [] -> []
// shuffle [1;2] -> [1;2]
// shuffle [1..4] -> [1;4;2;3]

let rec shuffle list =
    match list with
    | [] -> []
    | h :: t -> h :: shuffle (List.rev t)

shuffle [1..4]

// 3. Make a function segments: int list -> int list list that splits the list passed
// as an argument into list of lists of nondecreasing segments.
// The segments need to be of maximal possible length (the number of segments
// needs to be minimal)
// E.g.
// segments [] ->  []
// segments [1] -> [[1]]
// segments [3;4;5;5;1;2;3] -> [[3;4;5;5];[1;2;3]]

let segments (integerList: int list) =
    List.foldBack (fun element list ->
        match list with
        | [] -> [[element]]
        | (h1::h2)::t when element <= h1 -> (element::h1::h2)::t
        | _ -> [element]::list
    )  integerList []

segments [3;4;5;5;1;2;3]

// 4. Make a function sumSublists : int list list -> int list that will compute the sums of sublists in a list of list of ints.
// Hint: use the function List.fold to compute the sums of lists of ints.

let rec sumSublists listOfLists =
    match listOfLists with
    | [] -> []
    | h::t -> (List.fold (+) 0 h) :: sumSublists t

sumSublists [[1;2;4];[2;3;20];[3;4;6]]
// 5. Write a function filterSegments : (int list -> bool) -> int list list -> int list list that will filter out lists of ints
// based on some filter function. Write a filter function for even sum, odd sum, even number of elements, odd number of elements.

let listOfLists = [[1;2;4];[2;3;20];[3;4;5];[10;2]]
let filterSegments (filterFunction: int list -> bool) (listOfLists: int list list) =
    listOfLists |> List.filter (fun (l) -> filterFunction l)

let evenSum (list: int list) = 
    (List.fold (+) 0 list) % 2 <> 0

let filterEvenSum = evenSum
filterSegments filterEvenSum listOfLists

let oddSum (list: int list) = 
    (List.fold (+) 0 list) % 2 = 0
let filterOddSum = oddSum
filterSegments filterOddSum listOfLists

let evenElements (list: int list) =
    list.Length % 2 <> 0
let filterEvenEl = evenElements
filterSegments filterEvenEl listOfLists

let oddElements (list: int list) =
    list.Length % 2 = 0
let filterOddEl = oddElements
filterSegments filterOddEl listOfLists