// Learn more about F# at http://fsharp.org

module LCA

type Node = {
    data: string
    left: Node option
    right: Node option
    parent: Node option
}

let buildTestTree = 
    let n1 = { data="1"; left=None; right=None; parent=None }
    let n2 = { data="2"; left=None; right=None; parent = Some n1 }
    let n3 = { data="3"; left=None; right=None; parent = Some n1 }
    let n4 = { data="4"; left=None; right=None; parent = Some n2 }

    // This won't work - need a better way of building the tree...
    let n1 = { n1 with left = Some n2; right = Some n3 }
    let n2 = { n2 with left = Some n4 }

    n1

[<EntryPoint>]
let main _ =
    printfn "Hello World from F#!"
    0 // return an integer exit code
