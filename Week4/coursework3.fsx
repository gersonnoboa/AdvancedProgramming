(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: User defined types

  ------------------------------------
  Name: Luis Noboa
  TUT Student ID: lunobo
  ------------------------------------


  Answer the questions below.  You answers to questions 1--7 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 9, 2015.
*)

// 1. Consider expression trees of type ExprTree declared in the lecture.
// Extend the type with if-then-else expression of the form:
// if b then e1 else e2
// where b is a boolean expression and e1 and e2 are expressions.
// An example expression could be:
// if a+3 > b+c && a>0 then c+d else e

type ExprTree = | Const  of int
                | Ident  of string
                | Minus  of ExprTree
                | Sum    of ExprTree * ExprTree
                | Diff   of ExprTree * ExprTree
                | Prod   of ExprTree * ExprTree
                | Let    of string * ExprTree * ExprTree
                | If     of BooleanTree * ExprTree * ExprTree

and BooleanTree = | Equal of ExprTree * ExprTree
                  | NotEqual of ExprTree * ExprTree
                  | GreaterThan of ExprTree * ExprTree
                  | GreaterThanOrEqual of ExprTree * ExprTree
                  | LessThan of ExprTree * ExprTree
                  | LessThanOrEqual of ExprTree * ExprTree
                  | And of BooleanTree * BooleanTree
                  | Or of BooleanTree * BooleanTree
                  | Not of BooleanTree

// 2. Extend the function eval defined in the lecture to support the
// if-then-else expressions defined in Q1.

let rec eval t env =
    match t with
    | Const n        -> n
    | Ident s        -> Map.find s env
    | Minus t        -> - (eval t env)
    | Sum (t1,t2)    -> eval t1 env + eval t2 env
    | Diff (t1,t2)   -> eval t1 env - eval t2 env
    | Prod (t1,t2)   -> eval t1 env * eval t2 env
    | If (s,t1,t2)    -> if evalBool s env = true then eval t1 env else eval t2 env
    | Let (s,t1,t2)  -> let v1 = eval t1 env
                        let env1 = Map.add s v1 env
                        eval t2 env1    

and evalBool t env =
    match t with
    | Equal(t1,t2)              -> eval t1 env = eval t2 env
    | NotEqual(t1,t2)           -> eval t1 env <> eval t2 env
    | GreaterThan(t1,t2)        -> eval t1 env > eval t2 env
    | GreaterThanOrEqual(t1,t2) -> eval t1 env >= eval t2 env
    | LessThan(t1,t2)           -> eval t1 env < eval t2 env
    | LessThanOrEqual(t1,t2)    -> eval t1 env <= eval t2 env
    | And(t1,t2)                -> evalBool t1 env && evalBool t2 env
    | Or(t1,t2)                 -> evalBool t1 env || evalBool t2 env
    | Not(t1)                   -> not (evalBool t1 env)

// if a+3 > b+c && a>0 then c+d else e
//let expr = If(Equal(Const(4), Const(5)), Const(3), Const(2))
let firstAnd = GreaterThan(Sum(Ident("a"), Const(3)), Sum(Ident("b"), Ident("c")))
let secondAnd = GreaterThan(Ident("a"), Const(0))
let finalAnd = And(firstAnd, secondAnd)

let trueCondition = Sum(Ident("c"), Ident("d"))
let falseCondition = Ident("e")

let expr = If(finalAnd, trueCondition, falseCondition)

let map = Map.ofList[("a", 5);("b", 0);("c", 3);("d", 4);("e", 1)]
eval expr map

// 3-4: Given the type definition:
type BList =
    | BEmpty
    | Snoc of BList * int
//
// 3. Make the function filterB: (prop: int -> bool) BList -> BList that will return a list for the elements of which
// the function prop returns true.

let rec filterB (prop: int -> bool) (blist: BList) =
    match blist with
    | BEmpty -> BEmpty
    | Snoc(b,i) -> if prop i = true then Snoc(filterB prop b, i) else filterB prop b

filterB (fun x -> 1<x) (Snoc(Snoc(Snoc(Snoc(Snoc(BEmpty, 4), 6), 2), 1), 45))

// 4. Make the function mapB: (trans: int -> int) BList -> BList that will return a list where the function trans has
// been applied to each element.

let rec mapB (trans: int -> int) (blist: BList) =
    match blist with
    | BEmpty -> BEmpty
    | Snoc(b,i) -> Snoc(mapB trans b, trans i)

mapB (fun x -> x+1) (Snoc(Snoc(Snoc(Snoc(BEmpty, 4), -6), 2), 1))

// 5-7. Given the type definition
type Tree =
  | Nil
  | Branch2 of Tree * int * Tree
  | Branch3 of Tree * int * Tree * int * Tree
//
// 5. Define the value exampleTree : Tree that represents the following
//    tree:
//
//        2
//       / \
//      *  3 5
//        / | \
//       *  *  *

let exampleTree = Branch2(Nil, 2, Branch3(Nil, 3, Nil, 5, Nil))

// 6. Define a function sumTree : Tree -> int that computes the sum of
//    all labels in the given tree.

let rec sumTree tree =
    match tree with
    | Nil -> 0
    | Branch2(t1, i1, t2) -> sumTree t1 + i1 + sumTree t2
    | Branch3(t1, i1, t2, i2, t3) -> sumTree t1 + i1 + sumTree t2 + i2 + sumTree t3

sumTree exampleTree

// 7. Define a function productTree : Tree -> int that computes the
//    product of all labels in the given tree. If this function
//    encounters a label 0, it shall not look at any further labels, but
//    return 0 right away.

let rec productTree tree =
    match tree with
    | Nil -> 1
    | Branch2(t1, i1, t2) -> if (i1 = 0) then 0 
                             else (productTree t1) * i1 * (productTree t2)
    | Branch3(t1, i1, t2, i2, t3) -> if (i1 = 0) || (i2 = 0) then 0 
                                     else (productTree t1) * i1 * (productTree t2) * i2 * (productTree t3)

productTree exampleTree
// ** Bonus questions **

// 8. Extend the ExprTree type with a pattern match expression
// match p with [p1, ex1; p2,ex2 ...]

// 9. Extend the eval function to support match expressions.
