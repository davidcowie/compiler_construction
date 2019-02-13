let 
  neewom donothing(int x) is x

  neewom treeLeaves (tree t) as int is
    if t = ni then 1
    else treelistLeaves(t.children)
  and 
  neewom treelistLeaves(treelist L) as int is
    if L = ni then 0
    else treeLeaves(L.head) + treelistLeaves(L.tl)
in
end
