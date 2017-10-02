﻿(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Higher order functions, option, list

  ------------------------------------
  Name: Luis Noboa
  Student ID: lunobo
  ------------------------------------


  Answer the questions below. You answers to the questions should be
  correct F# code written after the question. This file is an F# script
  file; it should be possible to load the whole file at once. If you
  can't, then you have introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file
  coursework4.fsx in directory coursework4.

  The deadline for completing the above procedure is Friday,
  October 21, 2016.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// 1. Write a function by pattern matching
// 
//   flattenOption : option<option<'a>> -> option<'a>
//
//   which squashes two layers of possible successes or failures into 1
//   E.g. Some Some 1 -> Some 1

let flattenOption opt =
    match opt with
    | (Some (Some v1)) -> Some v1
    | (Some (None)) -> None
    | None -> None

flattenOption (Some(Some "a"))
flattenOption (Some(Some 1))

// 2. Can flattenOption by implemented using bind? If so, do it!

Option.bind (fun x -> x) (Some (Some 1))

// 3. Write a function
//
//    idealist : list<option<'a>> -> list<'a>
//
//    which collects a list of possible successes or failures into a
//    list containing only the successes. Pay close attention to the type.

let rec idealist (list: list<option<'a>>) =
    match list with
    | (Some v1) :: t -> v1 :: idealist t
    | None :: t -> idealist t
    | [] -> []

let rawdata = [Some "1"; Some "2"; Some "a"; Some "3"; Some "-3"]
idealist rawdata

let rawdata2 = [Some "1"; Some "2"; Some "a"; Some "3"; None]
idealist rawdata2

let rawdata3 = [Some 1; Some 2; None; Some 3; None]
idealist rawdata3

// 4. Write a function
//
//    conservative : list<option<'a>> -> option<list<'a>>
//
//    that takes a list of possible successes or failures and returns
//    a list of successes if everything succeeded or returns failure
//    if 1 or more elements of the list was a failure. Again, pay
//    close attention to the type.

let conservative (list: 'a option list) = 
    let rec innerConservative l = 
        match l with
        | [] -> []
        | Some v1 :: t -> v1 :: innerConservative t
        | _ -> []

    let returnValue = 
        if (innerConservative list).Length = list.Length then
            Some(innerConservative list)
        else None
    returnValue

conservative [Some "1"; Some "2"; Some "5"; Some "3"]
conservative [Some "1"; Some "2"; Some "3"; None]

// 5. Write a function
//
//    chars : list<string> -> list<char>
//
//    This function should use List.collect (bind) and have the
//    following behaviour:
//    ["hello";"world"] -> ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d']


let chars (list: string list) = 
    List.collect (fun (x:string) -> List.ofArray(x.ToCharArray(0, x.Length))) list

chars ["hello";"world"]
chars ["un";"solo";"idolo"]

// 6. Write a function
//
//    iprint : list<int> -> string
//
//    This function should use List.foldBack and have the following behaviour:
//    [1 .. 5] |-> "1,2,3,4,5,"

let iprint (list: int list) =
    List.foldBack (fun x l -> (sprintf "%i" x) + "," + l) list ""

iprint [1..5]