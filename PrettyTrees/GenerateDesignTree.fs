//S204423 Anders Keller Poulsen
//S204452 Mathias Rerup-Dyberg
//S183879 Markus Jegstrup
//S112506 Michael Andersen
//Date: 13/06/2024
module GenerateDesignTree
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


let rec fit (ps : Extent) (qs : Extent) : float =
    match (ps, qs) with
    | ((_, p)::ps', (q, _)::qs') -> max (fit ps' qs')  (p - q + 1.0)
    | _ -> 0.0


let fitListL (es : Extent list) =
    let rec fitListL' (acc : Extent) (es : Extent list) =
        match es with
        | [] -> []
        | e::es' ->
            let x = fit acc e
            x :: fitListL' (merge acc (moveExtent e x)) es'
    fitListL' [] es

let fitListR (es : Extent list) =
    let rec fitListR' (acc : Extent) (es : Extent list) =
        match es with
        | [] -> []
        | e::es' ->
            let x = -(fit e acc)
            x :: fitListR' (merge (moveExtent e x) acc) es'
    in
        List.rev (fitListR' [] (List.rev es))

let mean (x, y) = (x + y) / 2.0

let fitList es = List.map mean (List.zip (fitListL es) (fitListR es))

let design tree =
    let rec design' (Node(label, subtrees)) =
        let trees, extents = List.unzip (List.map design' subtrees)
        let positions = fitList extents
        let ptrees = List.map2 moveTree trees positions
        let pextents = List.map2 moveExtent extents positions
        let resultextent = (0.0, 0.0) :: mergeList pextents
        let resulttree = Node((label, 0.0), ptrees)
        (resulttree, resultextent)
    fst (design' tree)
