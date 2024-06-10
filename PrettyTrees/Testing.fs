module Testing
open GenerateDesignTree
open FsCheck

let rec get_positions_level (Node(_, subtrees)) =
    match subtrees with
    | [] -> [[]]
    | _ ->
        let current_level = subtrees |> List.map (fun (Node((_, pos), _)) -> pos)
        let next_levels = subtrees |> List.collect get_positions_level
        current_level :: next_levels

// Check that nodes are at least one unit apart on each level
let checkPositions tree =
    let positions_by_level = get_positions_level tree
    let rec check_positions = function
        | [] -> true
        | level::rest ->
            let rec check_level = function
                | [] | [_] -> true
                | pos1::pos2::rest -> pos1 + 1.0 <= pos2 && check_level (pos2::rest)
            check_level level && check_positions rest
    check_positions positions_by_level

let ``Nodes should be at least one unit apart`` tree =
    let designedTree = design tree
    checkPositions designedTree

//let _ = Check.Quick ``Nodes should be at least one unit apart``

let ``Parent should be centered above its children`` tree =
    let designedTree = design tree
    let rec test tree =
        match tree with
        | Node (_, [])      -> true
        | Node((_, pos), subtrees) ->
            let childPositions = subtrees |> List.map (fun (Node((_, p), _)) -> p)
            let minPos = List.min childPositions
            let maxPos = List.max childPositions
            let mean = mean (minPos + pos, maxPos + pos)
            mean = pos && List.forall test subtrees
    test designedTree

//let _ = Check.Quick ``Parent should be centered above its children``
let rec reflect (Node(v, subtrees)) =
    Node(v, List.map reflect (List.rev subtrees))

let rec reflectPos (Node((v, x), subtrees)) =
    Node((v, -x), List.map reflectPos subtrees)
    

let ``Symmetric tree should be the same tree`` tree =
    let designedTree = design tree
    let temp = reflectPos (design (reflect tree))
    printfn "DesignTree: %A" designedTree
    printfn "ReflectedTree: %A" temp
    let pos1 = get_positions_level designedTree
    let pos2 = get_positions_level temp
    printfn "pos1: %A" pos1
    printfn "pos2: %A" pos2
    printfn "Equal %A" (pos1 = pos2)
    pos1 = pos2

let CompareTwoTrees (tree1 : Tree<'a * float>) (tree2 : Tree<'a>) : bool =
    
    let rec test (Node((label1, pos), children1)) (Node(_, children2)) =
        match children1, children2 with
        | _, []     -> true
        | [], _     -> true
        | _         -> tree1 = (design tree2) && List.forall2 test children1 children2
    test tree1 tree2

let ``Subtrees should be identical`` tree =
    let designedTree = design tree
    CompareTwoTrees designedTree tree
    
let runTests =
    Check.Quick ``Nodes should be at least one unit apart``
    Check.Quick ``Parent should be centered above its children``
    //Check.Quick ``Symmetric tree should be the same tree``
    Check.Quick ``Subtrees should be identical``