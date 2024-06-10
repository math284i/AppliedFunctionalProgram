module Visuals

open SharpVG
open System


let black, white = Color.ofName Black, Color.ofName White
let style = Style.create black white Length.one 1.0 1.0

let text = 
    Text.create (Point.ofInts (10, 10)) "a" 
    |> Element.createWithStyle style 
    |> Svg.ofElement



text |> Svg.toHtml "Square" |> printfn "%s"
