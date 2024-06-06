// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open FsCheck

let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs

Check.Quick revRevIsOrig

let revIsOrig (xs:list<int>) = List.rev xs = xs
Check.Quick revIsOrig