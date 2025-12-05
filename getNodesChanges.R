getNodesChanges <- function(tree) {
  require(phangorn)
  
  c((Ntip(tree)+1):(Nnode(tree)+Ntip(tree))) -> all.nodes
  tree$node.label -> node.states
  names(node.states) <- all.nodes
  node.states -> state.changes
  state.changes[] <- "no"
  
  for (i in 2:length(node.states)) {
    all.nodes[i] -> n0
    Ancestors(tree, n0, type="parent") -> p0
    node.states[match(n0, names(node.states))] -> s1
    node.states[match(p0, names(node.states))] -> s0
    if (s0 != s1) {
      state.changes[i] <- "yes"
    }
  }
  as.numeric(names(which(state.changes == "yes"))) -> change.nodes
  return(change.nodes)
}


