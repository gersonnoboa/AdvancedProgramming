// Here are some optional exercises, answer the questions below and
// make sure to write tests.

// 1. implement addition, multiplication, subtraction for Nat as
//    custom operators

type Nat =
  | Zero
  | Suc of Nat

let One = Suc Zero
let Two = Suc One
let Three = Suc Two
let Four = Suc Three

let rec (++) m n = 
   match m with
     | Zero   -> n
     | Suc m1 -> Suc ((++) m1 n) 

Two ++ One

let rec (--) m n = 
   match m,n with
     | Zero, Suc n1  -> failwith "Invalid operation, first Nat must be greater than or equal the second Nat"
     | _, Zero -> m
     | Suc m1, Suc n1 -> (--) m1 n1

Four -- One
Four -- Zero
Four -- Two
//Two -- Four
Two -- Two

let rec (|*|) m n =
    match m with
    | Zero -> Zero
    | Suc Zero -> n
    | Suc m1 -> n ++ ((|*|) m1 n)

Three |*| Four

// 2. Write a converstion function from Nat to int

let rec convertToInt nat =
    match nat with
    | Zero -> 0
    | Suc m1 -> 1 + (convertToInt m1)

convertToInt (Three |*| Four)

// 3. Write an evaluator for the following language of aritmetic expressions:

type Exp =
  | Val of Nat
  | Add of Exp * Exp
  | Sub of Exp * Exp
  | Mult of Exp * Exp

//    eval : Exp -> int

let rec eval exp =
    match exp with
    | Val v1 -> convertToInt v1
    | Add (v1, v2) -> eval v1 + eval v2
    | Sub (v1, v2) -> eval v1 - eval v2
    | Mult (v1, v2) -> eval v1 * eval v2

eval (Val(Four))
eval (Add(Val Two, Val Three))

// 4. Extend the language and the evaluator to support Sub, and Mult

eval (Sub(Val Two, Val Three))
eval (Mult(Val Three, Val Four))

// 5. Write an evaluator for this language which has variables as well.

type Exp<'t> =
  | Val of Nat
  | Var of 't
  | Add of Exp<'t> * Exp<'t>

//    The evaluator should take an lookup function too:
//    eval : ('t -> int) -> Exp<'t> -> int
//    (looking in environment ('t -> int), map, 't is character)
let rec eval2 lookup exp = 
    match exp with
    | Val v1 -> convertToInt v1
    | Var v1 -> lookup v1
    | Add (v1, v2) -> eval2 lookup v1 + eval2 lookup v2

let lookup char =
    Map.find char (Map.ofList[("a", 5);("b", 0);("c", 3);("d", 4);("e", 1)])

eval2 lookup (Val Three)
eval2 lookup (Var "b")
eval2 lookup (Add(Val Four, Var "e"))

// 6. Write a map function for Exp<'t>, it can be thought of a
//    'renaming' function that renames variables.

let rec rename exp func =
    match exp with
    | Val v1 -> Val v1
    | Var v1 -> Var(func v1)
    | Add (v1, v2) -> Add(rename v1 func, rename v2 func)
    
let func a = Map.find a (Map.ofList[("a", 2)])

rename (Var "a") func

// 7. Write a bind function (see section 6.8.2) for Exp<'t>, it can be
//    thought of as a substitution function that replaces variables with
//    expressions.

let lookupWith char map = Map.find char map
 
let rec bindExp list env = 
    let rec unwrap exp env =
        match exp with
        | Val v1 -> convertToInt v1
        | Var v1 -> unwrap(Map.find v1 env) env
        | Add(v1, v2) -> unwrap v1 env + unwrap v2 env

    match list with
    | [] -> []
    | Val v1 :: t -> unwrap (Val v1) env :: bindExp t env
    | Var v1 :: t -> unwrap (Var v1) env :: bindExp t env
    | Add (v1, v2) :: t -> unwrap (Add (v1, v2)) env :: bindExp t env

bindExp [Val Two; Var "a"; Add(Val Two, Var "a")] (Map.ofList[("a", Val Four)])