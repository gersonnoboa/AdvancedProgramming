(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 8: Sequences and computation expressions

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
  coursework8.fsx in directory coursework8.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, December 2, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Define a sequence powers : int seq that contains all powers of 2 in ascending
  order. Use Seq.unfold in your implementation.
*)

let powers =
    let step k = Some(k, 2 * k)
    Seq.unfold step 1

powers

(*
  Task 2:

  Define a sequence primes : int seq that contains all prime numbers in
  ascending order. Use sequence expressions in your implementation. You may want
  to use the function isPrime : int -> bool defined below. This function checks
  whether any given number that is greater or equal 2 is a prime number.
*)

let isPrime n =
  let rec hasDivisorFrom d n =
    if d * d <= n then
      if n % d = 0 then
        true
      else
        hasDivisorFrom (d + 1) n
    else
      false
  not (hasDivisorFrom 2 n)

let primes: seq<int> =
    let rec worker (num: int) =
        seq {
            if isPrime num then
                yield num
            yield! worker (num + 1)
        }
    worker 2

primes |> Seq.take(20) |> List.ofSeq

(*
  Task 3:

  Define a sequence primes' : int seq that again contains all prime numbers in
  ascending order. This time, do not use sequence expressions in your
  implementation, but use an appropriate function from the Seq module. Again,
  you may want to use the function isPrime : int -> bool defined above.
*)

let rec from n =
  seq {
    yield n
    yield! from (n + 1)
  }
 
let primes' =
    from 1 |> Seq.filter (fun x -> isPrime x)

primes' |> Seq.take 20 |> List.ofSeq

(*
  Task 4:

  Define a function fourthRoot : float -> float option that returns Some x if x
  is the 4th root of the argument, and None if the argument has no 4thآ root. In
  your implementation, use the squareRoot function from the lecture and
  computation expressions for the option type as defined in the lecture.
*)

type OptionBuilder () =
  member this.Bind   (opt, f) = Option.bind f opt
  member this.Return x        = Some x

let option = new OptionBuilder ()

let squareRoot x =
  if x >= 0.0 then Some (sqrt x) else None

let fourthRoot x = 
    option {
        let! firstRoot = squareRoot x
        let! secondRoot = squareRoot firstRoot
        return secondRoot
    }
    
fourthRoot -17.0

(*
  Task 5:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    â€¢ the definition of a builder that lets you express reader computations
      using computation expressions

    â€¢ the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    â€¢ the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    â€¢ the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> map <string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values. Use
  computation expressions for reader computations in your implementation. Note
  that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.
*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
  | Const of int
  | Ident of string
  | Neg   of Expr
  | Sum   of Expr * Expr
  | Diff  of Expr * Expr
  | Prod  of Expr * Expr
  | Let   of string * Expr * Expr

let rec eval expr map =
    if map = Map.ofList [] then
        0
    else
        match expr with
        | Const c -> c
        | Ident i -> Map.find i map
        | Neg n -> - (eval n map)
        | Sum (a, b) -> eval a map + eval b map
        | Diff (a, b) -> eval a map - eval b map
        | Prod (a, b) -> eval a map * eval b map
        | Let (a, b, c) -> let v1 = eval b map
                           let newEnv = Map.add a v1 map
                           eval c newEnv
    
//let expr = Prod(Ident "a", Sum (Neg (Const 3), Let ("x", Const 5, Sum (Ident "x", Ident "a"))))
let expr = Prod(Ident "d", Sum(Let("x", Const 6, Sum(Ident"x", Ident "b")) , Neg(Ident "e")))

let partialFunc map = eval expr

let envTest = Map.ofList[("a", 5);("b", 2);("c", 3);("d", 4);("e", 1)]
//let envGood = Map.add "a" -7 Map.empty
//let envBad : Map<string,int> = Map.ofList []
let env = ask envTest

let result = 
    reader{
        let! r = runReader partialFunc env
        return r
    }

result env