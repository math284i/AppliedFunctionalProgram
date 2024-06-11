module Render

open SharpVG
open GenerateDesignTree

// Defining the colors
let black = Color.ofName Black
let white = Color.ofName White

// Defining the styles
let lineStyle    = Style.createWithStroke black |> Style.withStrokeWidth (Length.ofInt 2)
let textStyle    = Style.createWithFill white
let textBoxStyle = Style.createWithFill black

let labelSize (text : string) =
    let charWidth = 12.0
    let width = (float text.Length) * charWidth + charWidth
    let height = 26.0
    (width, height)

let makeLabelText (x: float, y : float) (text : string) = 
    Text.create (Point.ofFloats (x, y + 7.0)) text
    |> Text.withFontFamily "Andale Mono, monospace"
    |> Text.withFontSize 20
    |> Text.withAnchor Middle
    |> Element.createWithStyle textStyle

let makeLabelBackground (x : float, y : float) (text : string) =
    let radius = 8.0
    let (w, h) as size = labelSize text
    let (x', y') = (x - w/2.0, y - h/2.0)

    Rect.create (Point.ofFloats (x', y')) (Area.ofFloats size)
    |> Rect.withCornerRadius (Point.ofFloats (radius, radius))
    |> Element.createWithStyle textBoxStyle

let makeLine a b = 
    Line.create (Point.ofFloats a) (Point.ofFloats b)
    |> Element.createWithStyle lineStyle

// Tail recursive function to create the line elements
let makeLineElements (Node((_, coords), subtrees)) =
    let rec aux acc p1 subtrees =
        let folder = fun acc (Node((_, p2), subtrees')) ->
            let newLine = makeLine p1 p2
            aux (newLine :: acc) p2 subtrees'
        List.fold folder acc subtrees
    aux [] coords subtrees

// Tail recursive function to create the label elements
let makeLabelElements tree =
    let rec aux acc (Node((label, coords), subtrees)) =
        let text = string label
        let acc' = makeLabelBackground coords text :: makeLabelText coords text :: acc
        List.fold aux acc' subtrees
    aux [] tree

let makeElements arrangedTree =
    makeLineElements arrangedTree @ makeLabelElements arrangedTree

let makeSvg (arrangedTree, size) =
  arrangedTree
  |> makeElements
  |> Svg.ofList
  |> Svg.withSize (Area.ofFloats size)

let makeHtml (arrangedTree, size) =
  let svg = makeSvg (arrangedTree, size)
  svg |> Svg.toHtml "Tree"

let storeHtml filePath (arrangedTree, size) =
  let html = makeHtml (arrangedTree, size)
  System.IO.File.WriteAllText(filePath, html)