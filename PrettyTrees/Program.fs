﻿// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

type Tree<'a> =
    | Node of 'a * Tree<'a> list

type Extent = (float*float) list

let moveTree (Node((label, x), subtrees) : Tree<'a * float>) (x' : float) : Tree<'a * float> =
    Node((label, x + x'), subtrees)

let moveExtent (e : Extent) (x : float) : Extent = 
    List.map(fun (p,q) -> (p+x,q+x)) e

let rec merge (ps: Extent) (qs : Extent) : Extent =
    match (ps, qs) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _)::ps, (_, q)::qs) -> (p, q) :: merge ps qs


let mergeList = List.fold merge []


let rmax (p : float) (q : float) : float =
    if p > q then p else q


let rec fit (ps : Extent) (qs : Extent) : float =
    match (ps, qs) with
    | ((_, p)::ps', (q, _)::qs') -> rmax (fit ps' qs')  (p - q + 1.0)
    | _ -> 0.0


let fitlistl (es : Extent list) =
    let rec fitlistl' (acc : Extent) (es : Extent list) =
        match es with
        | [] -> []
        | e::es' ->
            let x = fit acc e
            x :: fitlistl' (merge acc (moveExtent e x)) es'
    fitlistl' [] es

let fitlistr (es : Extent list) =
    let rec fitlistr' (acc : Extent) (es : Extent list) =
        match es with
        | [] -> []
        | e::es' ->
            let x = -(fit e acc)
            in
                x :: fitlistr' (merge (moveExtent e x) acc) es'
    in
        List.rev (fitlistr' [] (List.rev es))

let mean (x, y) = (x + y) / 2.0

let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es))

let design tree =
    let rec design' (Node(label, subtrees)) =
        let trees, extents = List.unzip (List.map design' subtrees)
        let positions = fitlist extents
        let ptrees = List.map2 moveTree trees positions
        let pextents = List.map2 moveExtent extents positions
        let resultextent = (0.0, 0.0) :: mergeList pextents
        let resulttree = Node((label, 0.0), ptrees)
        (resulttree, resultextent)
    fst (design' tree)


let tree = Node((1, 0.0), [
        Node((2, 0.0), []);
        Node((3, 0.0), []);
        Node((6, 0.0), []);
    ])

let tree2 = Node((1, 0.0), [
        Node((2, 0.0), []);
        Node((3, 0.0), [
            Node((7, 0.0), []);
            Node((8, 0.0), []);
            Node((9, 0.0), []);
        ]);
        Node((6, 0.0), [
            Node((7, 0.0), []);
            Node((8, 0.0), []);
            Node((9, 0.0), []);
        ]);
    ])




let result = design tree2

//printfn "%A" result

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
    let newTree = design tree
    checkPositions newTree

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

let _ = Check.Quick ``Parent should be centered above its children``




