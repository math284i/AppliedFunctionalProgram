module Visuals

open SharpVG
open GenerateDesignTree

// Defining the colors
let black = Color.ofName Black
let white = Color.ofName White

// Defining the styles
let lineStyle = Style.createWithPen Pen.black
                |> Style.withStrokeWidth (Length.ofInt 2)
let textStyle = Style.createWithFill white
let textBGStyle = Style.createWithFill black


let makeLabelText (x,y) (text : string) = 
  Text.create (Point.ofFloats (x, y + 7.0)) text
  |> Text.withFontFamily "Courier New"
  |> Text.withFontSize 20
  |> Text.withAnchor Middle
  |> Element.createWithStyle textStyle


let labelSize (text : string) =
  let charWidth = 12.0
  let width = (float text.Length) * charWidth + charWidth
  let height = 26.0 //12 is actual height and then 7 is added to the top and bottom to 
  (width, height)

let makeLabelBackground (x : float, y : float) (text : string) =
  let radius = 5.0
  let (w, h) as size = labelSize text
  let (x', y') = (x - w/2.0, y - h/2.0)

  Rect.create (Point.ofFloats (x', y')) (Area.ofFloats size)
  |> Rect.withCornerRadius (Point.ofFloats (radius, radius))
  |> Element.createWithStyle textBGStyle

let makeLabel coords (text : string) =
  [
    makeLabelBackground coords text
    makeLabelText coords text
  ]
  |> Group.ofList
  |> Element.createWithName "label"

let makeLine (x1,y1) (x2,y2) = 
  Line.create (Point.ofFloats (x1, y1)) (Point.ofFloats (x2, y2))
  |> Element.createWithStyle lineStyle

let rec makeLineElements (Node((_, coords), subtrees)) =
    let mapper (Node((_, coords'), _)) = makeLine coords coords'
    let lines = List.map mapper subtrees
    let lines' = List.collect makeLineElements subtrees
    lines @ lines'

let rec makeLabelElements (Node((label, coords), subtrees)) =
    let labelElement = makeLabel coords (string label)
    let labelElements = List.collect makeLabelElements subtrees
    labelElement :: labelElements

let makeElements arrangedTree =
  makeLineElements arrangedTree @ 
  makeLabelElements arrangedTree


let addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let multiplyCoords (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

let rec transformCoords fn (Node((label, cords), subtrees))  =
    let cords' = fn cords
    let subtrees' = List.map (transformCoords fn) subtrees
    Node((label, cords'), subtrees')

let offsetCoords coords = transformCoords (addCoords coords)
let scaleCoords coords = transformCoords (multiplyCoords coords)

let rec minX (Node((_, (x, _)), subtrees)) =
    subtrees |> List.map minX |> List.fold min x

let maxRect (w1, h1) (w2, h2) = (max w1 w2, max h1 h2)

let rec getMaxLabelSize (Node((label, _), subtrees)) =
    let size = labelSize (string label)
    List.map getMaxLabelSize subtrees |> List.fold maxRect size

let rec maxCoords (Node((_, coords), subtrees)) = 
  List.map maxCoords subtrees |> List.fold maxRect coords

// Calculates the coordinates of the nodes in the tree and fits them into a bounding box starting at (0,0)
let arrangeTree designedTree =
    // Convert the position to absolute x positions and y positions
    let rec convert (parentPos : float) (y : float) (Node((label, pos), subtrees)) =
        let x = parentPos + pos
        let subtrees' = List.map (convert x (y + 1.0)) subtrees
        Node((label, (x, y)), subtrees')
    
    let coordTree = convert 0 0 designedTree
    let moveX = minX coordTree
    coordTree |> offsetCoords (-moveX, 0.0)

let makeSvg tree =
  let designedTree = design tree
  let arrangedTree = arrangeTree designedTree

  let maxLabelSize = getMaxLabelSize arrangedTree

  // The scale must leave enough space for the labels
  let scale = maxRect maxLabelSize (100.0, 100.0)
  
  // The margin must leave room for the largest label
  let margin = multiplyCoords maxLabelSize (0.5, 0.5)
  
  // Scale the tree and offset it to make room for the labels
  let scaledTree =
    arrangedTree
    |> scaleCoords scale
    |> offsetCoords margin
  
  // The size is found by expanding a 
  let size = 
    maxCoords scaledTree 
    |> addCoords margin

  scaledTree
  |> makeElements
  |> Svg.ofList
  |> Svg.withSize (Area.ofFloats size)

let storeFile filePath tree =
  let html = tree |> makeSvg |> Svg.toHtml "Tree"
  System.IO.File.WriteAllText(filePath, html)
