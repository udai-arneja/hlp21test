module Answers

(* 
Q1. How many values does the q1Type type have? int8 is an 8 bit numeric type
(Q1 is a function that must return the answer)
*)

type q1Type = 
    | House of HasRoof: bool * FloorAreaM2: int8
    | Hovel of HasWalls: bool
    | Hut of IsBamboo: bool * IsWaterProof: bool

let q1() : int = failwith "Not answered"




(*
Q2. Property-based testing is - return the most correct answer:
0 = better than unit testing
1 = more complete than unit testing
2 = more able to find unexpected corner cases than unit testing
(Q2 is a function that must return the correct answer)
*)
let q2() : int = failwithf "Not answered"

(*
Q3. The Option type is preferable to using Null pointers because
A = It has a better implementation
B = It provides type protection
C = It documents program behaviour
0 = None of above
1 = A
2 = B
3 = C
4 = A & B
5 = A & C
6 = B & C
7 = A & B & C
(Q3 is a function that must return the correct answer)
*)

let q3() : int = failwithf "Not answered"


(*
Q4. Write a function (see below for signature) that returns a list whose nth
element is the nth element of lst added on the head of the nth element of lsts.
You may assume that lst and lsts have the same length.
Do not used indexes or List.item.
*)
let q4  (lsts: 'a list list) (lst: 'a list) : 'a list list =
    failwithf "Not answered"

//-----------------DO NOT CHANGE THIS--------------------------------//
/// function to use when implementing q4 if you have not correctly answered q4
let q4UsingIndexes (lsts: 'a list list) (lst: 'a list)  =
    printfn "lst=%A, lsts=%A" lst lsts
    [0..lst.Length-1]
    |> List.map (fun i -> lst.[i] :: lsts.[i])
//--------------------END OF DO NOT CHANGE---------------------------//


(*
Q5. Write a function, using your answer to q4 or (equivalent) q4usingIndexes, that
implements the transpose of a matrix represented as a list of lists where
each list represents one row and the ith element of each list the ith column.
You may assume the matrix is not empty.
Do not uses indexes or List.item
HINT: consider List.fold
*)
let q5 (lsts: 'a list list) : 'a list list = 
    failwithf "Not answered"
    

(*
Q6. Write a function that takes two ordered integer lists (ordered by increasing values),
    and return one list, which contains the values of both lists, and is also ordered by increasing values.
    You may only use the List.rev function from the list library, which takes a list and reverses its order.
    You may not use array functions.
    You should consider using a recursive function.
*)
let q6 (left: int list) (right: int list): int list =
    failwithf "Not answered"





(*
Q7 return the number of sets in setOfSets that el is an element of
*)
let q7 (el: int) (setOfSets: int Set Set) =
    failwithf "Not answered"

(*
Q8) Given a map and a value, return a Set of all the keys that are mapped to the value.
*)
let q8 (map: Map<'a, 'b>) (x: 'b): Set<'a> =
    failwithf "Not answered"
