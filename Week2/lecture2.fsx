

let number = 42

printfn "%d" number

let message = "Answer: " + number.ToString()

let number = 42 in
(
   printfn "%d" number;
   let message = "Answer: " + number.ToString() in printfn "%s" message
  
)

number.ToString()

let multiply num1 num2 = num1 * num2

multiply 2 3

let partialmultiply= multiply 2

partialmultiply 3

let division snd fst = fst / snd

let printSquares message num1 num2 =
  let printSquaresUtility num =
    let squared = num * num
    printfn "%s %d: %d" message num squared
  printSquaresUtility num1
  printSquaresUtility num2

printSquares "Square of " 14 27

let n1 = 22

let mutable n2 = 23

n2

let tp = "Hello world" , 42

let printCity cityInfo =
  printfn "Population of %s is %d" (fst cityInfo) (snd cityInfo)

let prague = "Prague", 1188126
let seattle = "Seattle", 594210

printCity prague

fst
snd

let withItem2 newItem2 tuple =
  let originalItem1, originalItem2 = tuple
  (originalItem1, newItem2)

let pragueNew = withItem2 (snd prague + 1000) prague

let withItem2 newItem2 tuple =
   match tuple with
   | originalItem1, _ -> originalItem1, newItem2

let setPopulation tuple newPopulation =
   match tuple with
   | "New York", _ -> "New York", newPopulation + 100
   | cityName, _ -> cityName, newPopulation


setPopulation ("New York",1243145) 1231550

setPopulation seattle 123

let rec factorial n =
  if n <= 1 then
    1
  else
    n * factorial (n - 1)

open System.Numerics

let rec factorial2 (n:System.Numerics.BigInteger) =
  if n <= BigInteger(1) then
    BigInteger(1)
  else
    n * factorial2 (n - BigInteger(1))


factorial2 (BigInteger(300))

let ls1 = []

let ls2 = 6 :: 2 :: 7 :: 3 :: []

let ls2 = [6;2;7;3]

let ls3 = [1..5]

let ls4 = 0 :: ls3

let squareFirst list = 
   match list with
   | head :: _ -> head * head
   | [] -> -1

//let squareFirst2 (head :: _) = 
//     head * head

squareFirst [4;5;6]

let rec sumList list =
  match list with
  | [] -> 0
  | head :: tail -> head + sumList tail

sumList [4..6]

let rec prodList list =
  match list with
  | [] -> 1
  | head :: tail -> head * prodList tail

prodList [4..6]

let rec aggregate (op: int -> int -> int) init list =
  match list with
  | [] -> init
  | head :: tail -> op head (aggregate op init tail)


2 + 3

(+) 2 3

(+) 2
// sum
aggregate (+) 0 [4..6]


let comp n =
   n < 6

List.filter comp [4..10]


// prod
aggregate (*) 1 [4..6]

let sumList1 = aggregate (+) 0

let addSpaces s1 s2 =
    s1 + " " + s2

let vs = ["a"; "quick"; "brown"; "fox"; "jumped"; "over"; "the"; "lazy"; "door"]
List.fold addSpaces " " vs
List.fold addSpaces (List.head vs) (List.tail vs)

//map

let rec mymap op list = 
    match list with
    | [] -> []
    | head::tail -> op head :: mymap op tail

let scale s x = s * x
mymap (scale 1000) [1..5]

open System
let stringToInt s =
    Int32.Parse(s)

mymap stringToInt ["1"; "2"; "3"]

List.map stringToInt ["1"; "2"; "3"]

let arr = [|"1"; "3"; "2"|]
arr.[0]


//repeat

let rec repeatString m s = 
    match m with
    | 0 -> ""
    | _ when m>0 -> s + repeatString (m-1) s
    | _ -> failwith "negative argument"

//repeatString -1 "blah"

let rec alternateLines n l1 l2 =
    match n with
    |0 -> ""
    |_ when n>0 -> l1 + "\n" + alternateLines (n-1) l2 l1
    |_ -> failwith "negative arguments"
    
let knitPattern m n =
    alternateLines n
        (repeatString m "XO")
        (repeatString m "OX")    

knitPattern 21 5

let rec alternateLines1 n l1 l2 =
    match n with
    | 0 -> []
    | _ when n>0 -> l1 :: alternateLines1 (n-1) l2 l1
    | _ -> failwith "negative argument"

let pearlKnit m n =
    String.concat "\n" (alternateLines1
                            n
                            (String.replicate m "XO")
                            (String.replicate m "OX")
                        )

pearlKnit 21 5

let rec zip (lista: 'a list) (listb: 'b list) : ('a * 'b) list = 
    match (lista, listb) with
    |[],_ -> []
    |_,[] -> []
    |(ha::ta),(hb::tb) -> (ha, hb) :: zip ta tb

zip[1..3][1..4]

let rec unzip (plist: ('a * 'b) list): ('a list) * ('b list) =
    match plist with
    | [] -> [],[]
    | (ha,hb)::pt ->
        let ta,tb = unzip pt
        (ha::ta), (hb::tb)

let rec unzip (plist: ('a * 'b) list): ('a list) * ('b list) =
    match plist with
    | [] -> [],[]
    | ph::pt ->
        let ha,hb = ph
        let ta,tb = unzip pt
        (ha::ta), (hb::tb)
unzip[(1,2);(2,3);(3,4)]