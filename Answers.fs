module Answers

(* 
Q1. How many values does the F# Unit type contain?
(Q1 is a function that must return the answer)
*)
let q1() : int = 1


(*
Q2. The F# type constructor -> has what associativity?
0 = left associative
1 = right associative
2 = associativity does not apply to type constructors
(Q2 is a function that must return the correct answer)
*)
let q2() : int = 1



(*
Q3. The output list is twice the length of the input list. Each input list element occurs in order
in the output, twice. E.g [1;2;5] -> [1;1;2;2;5;5].
You are not allowed to use list indexing (.[] or List.item) in your answer.
*)
let q3 (lst: int list) : int list = 
    if lst = [] then [] else 
        List.collect (fun el -> [el;el]) lst


(*
Q4. The output is the sum of all the elements in the input lists.
Recursive functions are not allowed in the answer.
*)
let q4 (lsts: int list list) : int = 
    List.sumBy List.sum lsts
  
    



(*
Q5. The output is the mode (element with maximum number of occurences) in the input list.
Thus [1;2;0;3;2;1;6;6;1] -> 1
If there is more than modal element the output should be the most positive of all such.
You may assume there is at least one element in the list.
*)
let q5 (lst: int list): int =
    lst
    |> List.countBy id
    |> List.maxBy (fun (num,count) -> (count,num))
    |> fst

(* 
Q6. List elements are numbered from 0.
Element n in the output list is the product of elements 2n and 2n+1 in the input list
If the input list has an odd number of elements then the last element of the output list
is the square of the last element of the input list.
You are not allowed to use list indexing (.[] or List.item) in your answer.
*)
let q6 (lst: int list): int list =
    let isOddLength = List.length lst % 2 = 1
    List.pairwise lst
    |> List.indexed
    |> List.filter (fun (i,_) -> i % 2 = 0)
    |> List.map (fun (_,(a,b)) -> a*b)
    |> (fun products -> products @ (if isOddLength then [List.last lst * List.last lst] else []))

let q6' (lst: int list): int list =
    List.chunkBySize 2 lst
    |> List.map (function | [a;b] -> a*b | [x] -> x*x | _ -> failwithf "What? can't happen!")

