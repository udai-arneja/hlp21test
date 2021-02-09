module Answers

(* 
Q1. How many values does the Q1Type type have?
(q1 is a function that must return the answer)
*)

type Q1SubType = {
    First: bool
    Second: bool * bool
    }

type q1Type = 
    | Thing of Q1SubType
    | NextThing
    | DoubleThing of Q1SubType * Q1SubType

let q1() : int = failwith "Not answered"


// ----------------------------Q2----------------------

let q2DemoMatch (y: int) =
    let a = 10
    match y with
    | a when true -> printfn "%d" a
    | _ -> printfn "No match"


(*
Q2. In q2DemoMatch
0 = The No Match case will under some circumstances be evaluated
1 = The No Match case will never be evaluated because "when true" guard makes its case always happen
2 = The No Match case will never be evaluated because the first case pattern matches for all values of y.
(q2 is a function that must return the correct answer)
*)
let q2() : int = failwithf "Not answered"

//----------------------------Q3--------------------------
(*
Q3. 
Pure functional programs, when compared with object oriented (OO) programs
0 = are easier to test because they do not contain loops
1 = are easier to test because functions do not have side effects
2 = are not always easier to test because OO programs contain objects with embedded state that makes tests more transparent

(q3 is a function that must return the correct answer)
*)

let q3() : int = failwithf "Not answered"

//------------------------------Q4---------------------------
(*
Q4. 

The two cases of the Discriminated Union type Result<'T1,'T2> type represent:
0 = valid results of Type T1 or nothing
1 = valid results of type T1 and T2, or errors of type string
2 = valid results of type T1 or errors of type T2
3 = valid results of type T1 or errors of type string

(q4 is a function that must return the correct answer)
*)

let q4() : int = failwithf "Not answered"

//---------------------------------Q5--------------------------

(*
Q5.
In otherwise pure F# programs with printf

0 = all functions have no side effects
1 = functions have side effects but the printf side effect is a monad and so pure functional
2 = functions have a possible printf side effect (printing something)

(q5 is a function that must return the correct answer)
*)

let q5() : int = failwithf "Not answered"


//----------------------------------Q6 --------------------------

let rec q6fun a b =
    let localQ6fun = q6fun
    if a = 0 then 4
    else if a < 0 then
        (*1*) q6fun  (a+1) b
    else if a = 1 then
        (*2*) q6fun (a-1) (b + (*4*) q6fun (a-1) b)
    else  (*8*) localQ6fun (a-1) (b+1)

(*

The recursive function q6fun has 4 recursive calls each tagged (just before the function) with an integer (1,2,4,8).
For each of these recursive calls, determine whetehr the call is tail recursive or not-ntail-recursive.
Add together the numbers next to each tail recusrive call. thus if you think all the calls are 
tail recursive you get 15. If you think no calls are tail recursive you get 0.
Return your sum as the value of q6()
*)

let q6() = failwithf "Not Answered"


//------------------------------------Q7---------------------------------------------

(*
Q7. 
Write a function q6 that returns a list containing every third element in the input list, which
can be of any length, without using recursion
[1;2;3] -> [3]
[1;2] -> [2]
[1;4;0;7;8;10;11] -> [0;10
*)
let q7 (lst: 'a list) : 'a list = failwithf "Not answered"

//----------------------------------------Q8-----------------------------------------


(*
Q8. 
Write a function that returns a list containing every third element in the input list, which
can be of any length, using recursion, and without using List or Array or Seq module functions.
Your answer need not be tail recursive.

[1;2;4] -> [4]
[1;2] -> []
[1;4;0;7;8;10;11] -> [0;10]
*)
let rec q8 (lst: 'a list) : 'a list = failwithf "Not answered"

//----------------------------------------Q9------------------------------------------   
(*
Q9. 
Write a function q8 that returns a single set of all the elements in set2 not in set1 and the elements in set1 not in set2.
Do NOT use your answer to Q12 in solving this problem.
*)
let q9 (set1: int Set) (set2: int Set) : int Set = failwithf "Not answered"

//---------------------------------------Q10-------------------------------------------

(*
Q10. 
Write using a function q9 that when given a list of functions returns the composition of them all.
[f1;f2;f3] returns the function that when given x returns f1(f2(f3(x)) - in that order.
You may use a full-recursive solution, or some other method.
*)
let q10 (fLst: ('a -> 'a) list) : 'a -> 'a = failwithf "Not answered"

//--------------------------------------Q11--------------------------------------------
    
(*
Q11.
Write a function Q10 that when given a list of options returns a list option that is None if all the list elements are None and
otherwise a list of the contents of each non-None list element in the same order as originally, with None elements deleted.

[] -> None
[ None, None] -> None
[Some 1; Some 3; None ; Some 4] -> [1;3;4]
*)
let q11 (lstOfOpts: 'a option list) : 'a list option = failwithf "Not answered"

//-------------------------------------Q12----------------------------------------------

(*
Q12
Write a function q11 that when given a list and a boolean:
If the boolean is false the function returns
"Zero" if the list is empty
"One" if  the list has one element
"Two" if the list has two elements
"Lots" if the list has more than one element.
If the boolean is true the function returns:
"Zero" if the list is empty
"One" if the list has one element
"Lots" if the list has 2 or 3 elements
"TooMany" if the list has more than 3 elements

50% of mark will be awarded for compact readable solutions.
*)
let q12 (lst: 'a list) (goodAtCounting: bool) : string = failwithf "Not answered"


//--------------------------------Q13---------------------------------------------

(*
Q13. 
    A union-find is a structure for representing disjoint sets.
    It takes the form of a map where each key is either:
     - mapped to itself, in which case it is called "representative member of its class"
     - mapped to a different element, in which case the representative member of its class is
       the representative member of the class of the element it is mapped to.
    For example the following map has two classes, represented by 2 and 3, respectively containing the
    values { 2, 4, 6 } and { 3, 5 }
    2 -> 2
    4 -> 2
    6 -> 4
    3 -> 3
    5 -> 3

    Write a function that takes an union-find map of integers and an integer x, and returns
    - (Some i) if i is the representative member of x's class
    - None if x is not in the map
*)
let q13 (unionFind: Map<int, int>) (x: int): int option =
    failwithf "Not answered"

//----------------------------------Q14-----------------------------------------------

(*
Q14. Write a function that takes a list of integers l and an integer n,
    and returns two lists : one which contains all the elements of l that are less or equal to n
    and one which contains all the elements of all that are strictly greater than n.
    The order in which the elements appear is not important
    Answer this question without using any function from the List or Array or Seq libraries
*)
let q14 (l: int list) (n: int): (int list * int list) =
    failwithf "Not Answered"










