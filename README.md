# examples

### Lowest Common Ancestor (LCA)
An F# implementation of the lowest common ancestor problem. Provides 3 solutions:
- *With a parent pointer* - build paths from the root to the nodes and find the lowest node where these paths overlap.
- *Without a parent pointer* - build paths from the root using depth first search and then apply the first solution's algorithm
- *Without a parent pointer* - search using a pruning heuristic: once we have found one of the nodes in the tree we don't need to consider any nodes below it, as the LCA must be either it or a node above it.