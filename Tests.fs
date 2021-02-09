module Tests
open Expecto
open TestLib


let validQ7Or8 =
    [
        [],[]
        [1],[]
        [1;2],[]
        [1;2;4],[4]
        [1;2;3;4;5],[3]
        [2;4;6;8;10;12],[6;12]
     ]

let validQ9 =
    [
        (Set [1;2], Set [1;2]), Set []
        (Set [1;2;3], Set [1;2;3;4]), Set [4]
        (Set [1;2;3;4], Set [1;2]), Set [3;4]
        (Set [1;3;5], Set [4;3;6]), Set [1;4;5;6]
    ]

let validQ10 n =
    let f1 = fun x -> x * 2
    let f2 = fun x -> x + 3
    let f3 = fun x -> x - 11
    [
        [], n
        [f1], f1 n
        [f1;f2], f1(f2 n)
        [f1;f2;f3],f1(f2(f3 n))
        [f3;f1], f3 (f1 n)       
    ]

let validQ11 =
    [
        [], None
        [Some 1], Some [1]
        [Some 2; Some 3; None; Some 4], Some [2;3;4]
        [None;None;None],None
    ]

let validQ12 =
    [ 
        ([], false), "Zero"
        ([1],false),"One"
        ([3;4],false),"Two"
        ([1;2;3],false),"Lots"
        ([1;2;3;4;5;6;7],false),"Lots"
        ([],true), "Zero"
        ([2],true),"One"
        ([3;4],true), "Lots"
        ([1;2;3], true), "Lots"
        ([1;2;3;4],true), "TooMany"
        ([1;2;3;4;5;6;7],true),"TooMany"
 ]


let validQ13 =
    [ (Map [ 2, 2; 4, 2; 6, 4; 3, 3; 5, 3 ], 4), Some 2
      (Map [ 2, 2; 4, 2; 6, 4; 3, 3; 5, 3 ], 5), Some 3
      (Map [ 2, 2; 4, 2; 6, 4; 3, 3; 5, 3 ], 3), Some 3
      (Map [ 2, 2; 4, 2; 6, 4; 3, 3; 5, 3 ], 12), None ]

let validQ14 =
    [ ([], 0), ([], [])
      ([ 3; 3443; 545; 54; 4554; 3434 ], 100), ([ 3; 54 ], [ 3443; 545; 4554; 3434 ])
    ]

let mutable testResults: Map<string,bool list> = Map.empty

let recordTest name ok =
    let sofar = 
        Map.tryFind name testResults
        |> Option.defaultValue []
    testResults <- Map.add name (ok :: sofar) testResults

let testQ14' i q qStr ((l, n), (left, right)) = testCase (sprintf "Case%d" i) <| fun () -> 
    let (resLeft, resRight) = q l n
    let equalOrdered a b =
        (List.compareWith compare (List.sort a) (List.sort b)) = 0
    ((equalOrdered resLeft left)
    && (equalOrdered resRight right))
    |> fun eq -> 
        recordTest qStr eq
        Expect.isTrue eq (sprintf "%s %A %A -> %A %A" qStr l n left right)
   
let testQ14 q qStr valid =
    Expecto.Test.TestList (List.mapi ( fun i x -> testQ14' i q qStr x) validQ14, Normal)

let testBinary q qStr pairs=
    let testOneCase ((left, right), expected) =
        testCase (sprintf "Evaluating:\'%s (%A) (%A)\', expecting:\'%A\'" qStr left right expected) <| fun () ->
            let actual = q left right
            recordTest qStr (actual = expected)
            (Expect.equal actual expected "")
    testList (qStr+" List") (List.map testOneCase pairs)


let testUnary q qStr pairs =
    let testOneCase (arg, expected) =
        testCase (sprintf "Evaluating: \'%s (%A)\', expecting: \'%A\'" qStr arg expected) <| fun () ->
            let actual = q arg
            recordTest qStr (actual = expected)
            (Expect.equal actual expected qStr)
    testList (qStr+" List") (List.map testOneCase pairs)     

let getResult = function | [] -> false | oks -> List.reduce (&&) oks
        
let printTotals() =
    let res =
        testResults
        |> Map.toList
        |> List.map (fun (name,oks) -> name, getResult oks)
        |> List.sortBy (fun (s,_)-> String.filter ((<>) 'q') s |> int)
    res
    |> List.iter (fun (name,ok) ->
        printfn "%4s  %s" name (if ok then "Passed" else "Failed"))
    let total = List.length res
    let totalOk =
        List.filter (snd >> (=) true) res
        |> List.length
    printfn "Total (not counting MCQ questions) %d out of %d " totalOk total


[<Tests>]    
let tests =
    testList "HLP Test 2021" [
        checkMCQ [] Answers.q1 "q1"
        checkMCQ [0..2] Answers.q2 "q2"
        checkMCQ [0..2] Answers.q3 "q3"
        checkMCQ [0..2] Answers.q4 "q4"
        checkMCQ [0..2] Answers.q5 "q5" 
        checkMCQ [0..15] Answers.q6 "q6"
        testUnary Answers.q7 "q7" validQ7Or8
        testUnary Answers.q8 "q8" validQ7Or8
        testBinary Answers.q9 "q9" validQ9
        testUnary (fun fl -> Answers.q10 fl 100) "q10" (validQ10 100)
        testUnary Answers.q11 "q11" validQ11
        testBinary Answers.q12 "q12" validQ12
        testBinary Answers.q13 "q13" validQ13
        testQ14 Answers.q14 "q14" validQ14
    ]


