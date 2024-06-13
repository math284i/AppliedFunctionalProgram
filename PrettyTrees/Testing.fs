module Testing
open FsCheck.FSharp
open GenerateDesignTree
open FsCheck

let rec get_positions_level (Node(_, subtrees)) =
    match subtrees with
    | [] -> [[]]
    | _ ->
        let current_level = subtrees |> List.map (fun (Node((_, pos), _)) -> pos)
        let next_levels = subtrees |> List.collect get_positions_level
        current_level :: next_levels

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

let rec reflect (Node(v, subtrees)) =
    Node(v, List.map reflect (List.rev subtrees))

let rec reflectPos (Node((v, x), subtrees)) =
    Node((v, -x), List.map reflectPos subtrees)

let GetSubTree (Node(_, subTree)) = subTree

let CompareTwoTreesSubTree (tree1 : Tree<'a * float>) (tree2 : Tree<'a>) : bool =
    let rec test subTree1 subTree2 =
        let children1 = GetSubTree subTree1
        let children2 = GetSubTree subTree2
        match children1, children2 with
        | [], []    -> true
        | _, []     -> false
        | [], _     -> false
        | _         ->
            let designed = design subTree2
            let newChildren = GetSubTree designed
            children1 = newChildren && List.forall2 test children1 children2
    test tree1 tree2

let CompareTwoTreesLabels (tree1 : Tree<'a * float>) (tree2 : Tree<'a>) : bool =
    let rec test (Node((label1, pos), children1)) (Node((label2), children2)) =
        match children1, children2 with
        | [], []        -> true
        | _, []         -> false
        | [], _         -> false
        | _             -> label1 = label2 && List.forall2 test children1 children2
    test tree1 tree2

    
//Property 1    
let ``Nodes should be at least one unit apart`` tree =
    let designedTree = design tree
    checkPositions designedTree
    |> Prop.trivial ((GetSubTree tree).Length = 0) 
    |> Prop.classify (List.forall (fun (x:float list) -> x.Length < 2) (get_positions_level designedTree)) "Single element per level"
   
//Property 2   
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
    |> Prop.trivial ((GetSubTree designedTree).Length = 0)
    
//Property 3    
let ``Symmetric tree should be the same tree`` tree =
    let designedTree = design tree
    let reflectedTree = reflect (reflectPos (design (reflect tree)))
    let pos1 = get_positions_level designedTree
    let pos2 = get_positions_level reflectedTree
    pos1 = pos2
    |> Prop.trivial ((GetSubTree designedTree).Length = 0)
    |> Prop.classify (List.forall (fun (x:float list) -> x.Length < 2) (get_positions_level designedTree)) "Single element per level"
    
//Property 4        
let ``Subtrees should be identical`` tree =
    let designedTree = design tree
    CompareTwoTreesSubTree designedTree tree
    |> Prop.trivial ((GetSubTree tree).Length = 0)

//Property 5    
let ``Labels should stay the same after design`` tree =
    let designedTree = design tree
    CompareTwoTreesLabels designedTree tree
    
//Property 6    
let ``Design tree should be deterministic`` tree =
    let designedTree1 = design tree
    let designedTree2 = design tree
    designedTree1 = designedTree2
    
//Property 7
let ``Design won't affect length of tree list's`` tree =
    let designedTree = design tree
    let rec test tree1 tree2 =
        let children1 = GetSubTree tree1
        let children2 = GetSubTree tree2
        match children1, children2 with
        | [], []        -> true
        | _, []         -> false
        | [], _         -> false
        | _             -> children1.Length = children2.Length && List.forall2 test children1 children2
    test designedTree tree
    
let runTests =
    Check.Quick ``Nodes should be at least one unit apart``
    Check.Quick ``Parent should be centered above its children``
    Check.Quick ``Symmetric tree should be the same tree``
    Check.Quick ``Subtrees should be identical``
    Check.Quick ``Labels should stay the same after design``
    Check.Quick ``Design tree should be deterministic``
    Check.Quick ``Design won't affect length of tree list's``