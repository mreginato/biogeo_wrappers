getTipsChanges <- function(tree, tip.states) {
  require(phangorn)
  
  c((Ntip(tree)+1):(Nnode(tree)+Ntip(tree))) -> all.nodes
  c(1:Ntip(tree)) -> all.tips
  tree$node.label -> node.states
  names(node.states) <- all.nodes
  names(tip.states) <- all.tips
  c(tip.states, node.states) -> all.states
  c(all.tips, all.nodes) -> all.nodes
  tip.states -> state.changes
  state.changes[] <- "no"
  
  
  for (i in 1:length(tip.states)) {
    all.tips[i] -> n0
    Ancestors(tree, n0, type="parent") -> p0
    all.states[n0] -> s1
    all.states[p0] -> s0
    if (s0 != s1) {
      state.changes[i] <- "yes"
    }
  }
  as.numeric(names(which(state.changes == "yes"))) -> change.nodes
  return(change.nodes)
}


