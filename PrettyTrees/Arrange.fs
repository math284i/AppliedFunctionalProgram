module Arrange

open GenerateDesignTree

type Coords = float * float
type ArrangedTree = Tree<string * Coords>

// Convert to ArrangedTree where:
// - labels are converted to string
// - absolute coordinates are calculated using pos and depth
let rec private convert (parentPos : float) (depth : float) (Node((label, pos), subtrees) : Tree<'a * float>) : ArrangedTree =
    let x = parentPos + pos
    let subtrees' = List.map (convert x (depth + 1.0)) subtrees
    Node((string label, (x, depth)), subtrees')

let private maxRect (w1, h1) (w2, h2) =
    (max w1 w2, max h1 h2)

let rec private getMaxLabelSize (Node((label, _), subtrees) : ArrangedTree) : Coords =
    List.map getMaxLabelSize subtrees |> List.fold maxRect (Render.labelSize label)

let rec private scaleTree ((fx : float, fy : float) as factor) (Node((label, (x, y)), subtrees) : ArrangedTree) =
    Node((label, (x * fx, y * fy)), List.map (scaleTree factor) subtrees)

let private scale (tree : ArrangedTree) : ArrangedTree =
    let factor = getMaxLabelSize tree |> maxRect (100.0, 100.0)
    scaleTree factor tree

let arrangeTree (tree : Tree<'a * float>) =
    tree |> convert 0 0 |> scale

