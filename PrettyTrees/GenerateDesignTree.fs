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
