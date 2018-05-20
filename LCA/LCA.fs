module LCA

[<StructuredFormatDisplay("Node {name}")>]
type Node = {
    name : string
    left : Node option
    right : Node option
    parent : Node option ref
}

module Node =
    let parent node = !node.parent

let private isSameObj = LanguagePrimitives.PhysicalEquality

// Method 1: use the parent pointer to build two paths to the root
let rec private makePath fromNode =
    match fromNode with
    | None -> []
    | Some n -> n :: makePath !n.parent

let private lastEqualElement listA listB =
    let restrictLength = min (List.length listA) (List.length listB) |> List.take 
    
    // Zip the two paths from the root to nodes a and b together
    List.zip (restrictLength listA) (restrictLength listB) 
    // Filter to nodes only on both paths
    |> List.filter (fun (u,v) -> isSameObj u v)
    // Take the lowest such pair of nodes
    |> List.last
    // Return the node (either will do)
    |> (fun (u,_) -> u) 

let lca a b = 
    printfn "Nodes %A %A" a b
    let pathToA = Some a |> makePath |> List.rev
    printfn "Path to A %A" pathToA
    let pathToB = Some b |> makePath |> List.rev
    printfn "Path to B %A" pathToB
    
    lastEqualElement pathToA pathToB

// Method 2: search the tree for the nodes to build two paths to the root 
let rec private findNode node root =
    match root with
    | None -> []
    | Some r when isSameObj node r -> [r]
    | Some r ->
        let searchLeft = findNode node r.left
        if searchLeft <> [] then r :: searchLeft
        else
            let searchRight = findNode node r.right
            if searchRight <> [] then r :: searchRight else []

let lca2 a b root =
    let pathToA = Some root |> findNode a
    pathToA |> printfn "Path to A: %A"
    let pathToB = Some root |> findNode b
    pathToB |> printfn "Path to B: %A"

    lastEqualElement pathToA pathToB

// Method 3: search for the nodes - use the fact that once we're found a node we don't need
// to search any further. 
let rec private lca3inner a b root =
    match root with 
    | None -> None
    | Some r when isSameObj r a || isSameObj r b -> root
    | Some r ->
        let leftSearch = r.left |> lca3inner a b
        let rightSearch = r.right |> lca3inner a b

        match (leftSearch, rightSearch) with
        | (None, None) -> None // Neither a nor b are in this subtree
        | (Some n, None) -> Some n // One of the subtrees contains a or b
        | (None, Some n) -> Some n // Ditto
        | (Some _, Some _) -> root // One substree contains a and the other b - we are the LCA!

let lca3 a b root = 
    Some root |> lca3inner a b |> Option.get

let answer () =
    let n7 = { name = "7"; left = None; right = None; parent = ref None }
    let n8 = { name = "8"; left = None; right = None; parent = ref None }
    let n9 = { name = "9"; left = None; right = None; parent = ref None }
    
    let n4 = { name = "4"; left = Some n7; right = Some n8; parent = ref None }
    let n5 = { name = "5"; left = Some n9; right = None; parent = ref None }
    let n6 = { name = "6"; left = None; right = None; parent = ref None }
    
    let n2 = { name = "2"; left = Some n4; right = Some n5; parent = ref None }
    let n3 = { name = "3"; left = Some n6; right = None; parent = ref None }

    let n1 = { name = "1"; left = Some n2; right = Some n3; parent = ref None }

    n7.parent := Some n4
    n8.parent := Some n4
    n9.parent := Some n5

    n4.parent := Some n2
    n5.parent := Some n2
    n6.parent := Some n3

    n2.parent := Some n1
    n3.parent := Some n1

    let lca1 = lca n5 n8
    lca1 |> printfn "LCA method 1: %A"

    let lca2 = lca2 n5 n8 n1
    lca2 |> printfn "LCA method 2: %A"

    let lca3 = lca3 n5 n8 n1
    lca3 |> printfn "LCA method 3: %A"
    
