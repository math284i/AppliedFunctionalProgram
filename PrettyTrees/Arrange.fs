module Arrange

open GenerateDesignTree
open Render

let addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let multiplyCoords (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

let rec transformCoords fn (Node((label, cords), subtrees))  =
    let cords' = fn cords
    let subtrees' = List.map (transformCoords fn) subtrees
    Node((label, cords'), subtrees')

let offsetCoords coords = transformCoords (addCoords coords)
let scaleCoords coords = transformCoords (multiplyCoords coords)

// Convert the relative position to absolute x positions and y positions
let rec convert (parentPos : float) (y : float) (Node((label, pos), subtrees)) =
    let x = parentPos + pos
    let subtrees' = List.map (convert x (y + 1.0)) subtrees
    Node((label, (x, y)), subtrees')

let move coordTree =
    let rec minX (Node((_, (x, _)), subtrees)) =
        subtrees |> List.map minX |> List.fold min x
    let moveX = -minX coordTree
    coordTree |> offsetCoords (moveX, 0.0)

let maxRect (w1, h1) (w2, h2) =
    (max w1 w2, max h1 h2)

let rec maxCoords (Node((_, coords), subtrees)) = 
    List.map maxCoords subtrees |> List.fold maxRect coords

let rec getMaxLabelSize (Node((label, _), subtrees)) =
    let size = labelSize (string label)
    List.map getMaxLabelSize subtrees |> List.fold maxRect size

let arrangeTree designedTree =
    let maxLabelSize = getMaxLabelSize designedTree

    // The scale must leave enough space for the largest labels
    // next to each other
    let scale = maxRect maxLabelSize (100.0, 100.0)
    
    // The margin must leave room for the largest label
    // Since the labels are centered, the margin should half the max label size
    let margin = multiplyCoords maxLabelSize (0.5, 0.5)

    let arrangedTree = 
      convert 0 0 designedTree
      |> move 
      |> scaleCoords scale
      |> offsetCoords margin

    let size = 
      maxCoords arrangedTree 
      |> addCoords margin
    
    (arrangedTree, size)
