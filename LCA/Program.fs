module LCA

type Node = {
    data: string
    left: Node option
    right: Node option
    mutable parent: Node option
}

let emptyNode = { data="EMPTY"; left=None; right=None; parent=None }

//       1
//     2   3
//   4  5 6 
// 7  8 9

let buildTestTree = 
    let n9 = { data="9"; left=None; right=None; parent=None }
    let n8 = { data="8"; left=None; right=None; parent=None }
    let n7 = { data="7"; left=None; right=None; parent=None }
    let n6 = { data="6"; left=None; right=None; parent=None }
    let n5 = { data="5"; left=Some n9; right = None; parent= None }
    let n4 = { data="4"; left=Some n7; right = Some n8; parent=None }
    let n3 = {data="3"; left=Some n6; right = None; parent = None }
    let n2 = { data="2"; left = Some n4; right = Some n5; parent = None }
    let n1 = { data="1"; left = Some n2; right = Some n3; parent = None }

    n9.parent <- Some n6
    n8.parent <- Some n4
    n7.parent <- Some n4
    n6.parent <- Some n3
    n5.parent <- Some n2
    n4.parent <- Some n2
    n3.parent <- Some n1
    n2.parent <- Some n1

    n1

let rec lca nodeA nodeB (root: Node option) =
    printfn "Called with %A %A %A" nodeA.data nodeB.data root

    match root with
    | None -> None
    | Some r when r = nodeA || r = nodeB -> Some r
    | Some r -> 
        let leftResult = lca nodeA nodeB r.left
        match leftResult with
        | _ -> None
    //if Option.isNone root then None
    //else if root.Value = nodeA || root.Value = nodeB then root
    //else
    //    let leftResult = lca nodeA nodeB root.Value.left 
    //    let rightResult = lca nodeA nodeB root.Value.right 

        // if left is A or B then search right
    //    match leftResult, rightResult with
    //    | None, None -> None
    //    | Some l, Some r when l = nodeA && r = nodeB || l = nodeB && r = nodeA -> root
    //    | Some l, None -> Some l
    //    | None, Some r -> Some r
    //    | _,_-> None


[<EntryPoint>]
let main _ =
    //let tree = buildTestTree
    //let a = tree.left.Value.left.Value.right.Value
    //let b = tree.left.Value.right.Value
    let n3 = { emptyNode with data="3" }
    let n2 = { emptyNode with data="2" }
    let n1 = { emptyNode with data="1"; left=Some n2; right=Some n3 }
    n2.parent <- Some n1
    n3.parent <- Some n1
    
    let tree = n1
    let a = n2
    let b = n3

    printfn "%A" tree

    let lcaNode = Some tree |> lca a b

    printfn "%A" lcaNode
    0
