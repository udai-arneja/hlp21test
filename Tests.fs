module Tests
open Expecto
open TestLib


let validQ4Pairs  =
    [ 
        [1;2;3;4], [[];[];[];[]]
        [], []
        [1;0],[[2];[3]]       
    ] |> List.map (fun (a,b) -> (b,a), Answers.q4UsingIndexes b a)


let validQ5Pairs =
    [
        [[2]], [[2]]
        [[1;2]], [[1];[2]]
        [[1;8]; [2;5]], [[1;2]; [8;5]]
        [[1;2;3];[4;5;6]], [[1;4];[2;5];[3;6]]
    ]

let validQ6Pairs =
    [ ([], []), ([])
      ([1],[]), [1]
      ([],[1]), [1]
      ([ 1 .. 2 .. 9 ], [ 2 .. 2 .. 10 ]), ([ 1 .. 10 ])
      ([ 1 .. 10 ], [ 11 .. 20 ]), ([ 1 .. 20 ])
      ([ 1 .. 3 .. 9 ], [ 0 .. 2 .. 10 ]), ([ 0; 1; 2; 4; 4; 6; 7; 8; 10 ]) ]


let validQ7Pairs =
    let pSet = 
        [ [1] ; [2;3] ; [1;3]; [1;4;5]; [4;3]]
        |> List.map Set
        |> Set
    [
        (1, pSet), 3
        (0, pSet), 0
        (2, pSet), 1
        (4, pSet), 2
        (6, Set []), 0
    ]  
    

let validQ8Pairs =
    let m1 = [(0,2); (1,4); (2,4); (3,5)] |> Map.ofList
    let m2 = [] |> Map.ofList
    [
        (m1, 2), Set [0]
        (m1,0), Set []
        (m1,4), Set [1;2]
        (m2,0), Set []
    ]

let testBinary q qStr pairs=
    let testOneCase ((left, right), expected) =
        testCase (sprintf "Evaluating:\'%s (%A) (%A)\', expecting:\'%A\'" qStr left right expected) <| fun () ->
            let actual = q left right
            (Expect.equal actual expected "")
    testList qStr (List.map testOneCase pairs)


let testUnary q qStr pairs =
    let testOneCase (arg, expected) =
        testCase (sprintf "Evaluating: \'%s (%A)\', expecting: \'%A\'" qStr arg expected) <| fun () ->
            let actual = q arg
            (Expect.equal actual expected "")
    testList (qStr+" List") (List.map testOneCase pairs)     
        



[<Tests>]    
let tests =
    testList "Practice Test 2" [
        checkMCQ [0;1;2] Answers.q2 "Q2"
        checkMCQ [0;1;2;4;5;6;7] Answers.q3 "Q3"
        testBinary Answers.q4 "q4" validQ4Pairs
        testUnary Answers.q5 "q5" validQ5Pairs
        testBinary Answers.q6 "q6" validQ6Pairs
        testBinary Answers.q7 "q7" validQ7Pairs
        testBinary Answers.q8 "q8" validQ8Pairs
    ]


