// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

type Tree<'a> =
    | Node of 'a * Tree<'a> list

let moveTree (Node((label, x), subtrees) : Tree<string * float>) (x' : float) : Tree<string * float> =
    Node((label, x + x'), subtrees)
    
type Extent = (float*float) list

let moveExtent (e : Extent, x) = List.map(fun (p,q) -> (p+x,q+x)) e

let rec merge : Extent * Extent -> Extent = function
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _)::ps, (_, q)::qs) -> (p, q) :: merge (ps, qs)


let mergeList (es : Extent List) = List.fold (fun acc elem -> merge (acc, elem)) [] es













