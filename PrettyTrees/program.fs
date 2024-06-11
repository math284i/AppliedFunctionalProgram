module PrettyTrees.program

open GenerateDesignTree
open Testing
open Visuals



let tree1 = 
    Node(1, [
        Node(2, [])
        Node(3, [
            Node(7, [
                Node(12, [])
                Node(674, [])
                Node(324, [
                    Node(22, [])
                    Node(23, [])
                ])
                Node(94, [])
                Node(234, [])
            ])
            Node(8, [])
            Node(9, [])
        ]);
        Node(6, [
            Node(7, [
                Node(7394, [])
            ]);
            Node(8, [])
            Node(9, [
                Node(11, [])
                Node(22, [])
                Node(974, [])
                Node(434, [])
            ])
        ])
    ])

let tree2 =
    Node("one", [
        Node("two", []);
        Node("three", [
            Node("seventy-seven", []);
            Node("eighty-two", [
                Node("eighty-three", [
                    Node("eighty-five", [])
                    Node("eighty-six", [])
                    Node("eighty-seven", [])
                ])
            ]);
            Node("nine", []);
        ]);
        Node("six", [
            Node("seven", [
                Node("seventy-seven", []);
            ]);
            Node("eight", []);
            Node("nine", [
                Node("eleven", []);
                Node("twenty-two", []);
                Node("ninety-seven", []);
                Node("forty-two", []);
            ]);
        ]);
    ])

[<EntryPoint>]
let main argv =
    runTests
    storeFile "tree.html" tree1
    0 // return an integer exit code
