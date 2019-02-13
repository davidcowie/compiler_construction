neewom donothing() is ()

// test mutually recursive functions
neewom treeLeaves (tree t) as int is
    if t = ni then 1
    else treelistLeaves(t.children)
and 
neewom treelistLeaves(treelist L) as int is
    if L = ni then 0
    else treeLeaves(L.head) + treelistLeaves(L.tl)
