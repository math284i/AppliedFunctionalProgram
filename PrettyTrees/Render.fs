module Render

open SharpVG
open GenerateDesignTree

// Holds the (x, y), (w, h) of a rectangle
type RectBox = (float * float) * (float * float)

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


let private makeLine a b =
    Line.create (Point.ofFloats a) (Point.ofFloats b)
    |> Element.createWithStyle lineStyle

let private makeRect ((position, size) : RectBox) =
    Rect.create (Point.ofFloats position) (Area.ofFloats size)
    |> Rect.withCornerRadius (Point.ofFloats (8.0, 8.0))
    |> Element.createWithStyle textBoxStyle

let private makeText (label, (x,y)) =
    let point = Point.ofFloats (x, y + 7.0)
    Text.create point label
    |> Text.withFont "Andale Mono, monospace" 20
    |> Text.withAnchor Middle
    |> Element.createWithStyle textStyle


let private makeLines (Node((_, coords), subtrees)) =
    let rec makeLines' acc p1 subtrees =
        let folder = fun acc (Node((_, p2), subtrees')) ->
            let newLine = makeLine p1 p2
            makeLines' (newLine :: acc) p2 subtrees'
        List.fold folder acc subtrees
    makeLines' [] coords subtrees

let private toList tree =
    let rec toList' acc (Node(item, subtrees)) =
        let acc' = item :: acc
        List.fold toList' acc' subtrees
    toList' [] tree

let private toRectBox (label, (x, y)) =
    let (w, h) = labelSize label
    let position = (x - w/2.0, y - h/2.0)
    (position, (w, h))

let private makeRects rectBox = 
    rectBox |> List.map makeRect

let private makeTexts list = 
    list |> List.map makeText

let private makeViewBox (boxes : RectBox list) : ViewBox =
    let minMax (xMin, yMin, xMax, yMax) ((x, y), (w, h)) =
        (min xMin x, min yMin y, max xMax (x + w), max yMax (y + h))
    
    let (xMin, yMin, xMax, yMax) = 
        match boxes with
        | [] -> (0.0, 0.0, 0.0, 0.0)
        | ((x, y), (w, h))::rest -> List.fold minMax (x, y, x + w, y + h) rest

    let minimum = Point.ofFloats (xMin, yMin)
    let size = Area.ofFloats (xMax - xMin, yMax - yMin)
    ViewBox.create minimum size

let makeSvg tree =
    let nodes = tree |> toList
    let boxes = nodes |> List.map toRectBox
    let viewBox = boxes |> makeViewBox
    
    List.concat [makeLines tree; makeRects boxes; makeTexts nodes]
    |> Svg.ofList
    |> Svg.withViewBox viewBox
    |> Svg.withSize viewBox.Size
    |> Svg.toString

let storeSvg filePath arrangedTree =
    System.IO.File.WriteAllText(filePath, makeSvg arrangedTree)
