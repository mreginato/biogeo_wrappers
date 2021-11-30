getPlotDataBioGeoBEARS <- function(x, mat, areas.cols, range.col) {
  
  x -> biogeo
  mat -> tipStates
  
  for (i in 1:ncol(tipStates)) {
    tipStates[,i] -> tip0
    tip0[tip0 == 1] <-  areas.cols[i]
    tip0[tip0 == 0] <- "snow"
    tipStates[,i] <- tip0
  }
  
  tipranges = getranges_from_LagrangePHYLIP(lgdata_fn=biogeo$inputs$geogfn)
  areas.labs = getareas_from_tipranges_object(tipranges)
  statenames = areas_list_to_states_list_new(areas.labs, maxareas = biogeo$inputs$max_range_size, include_null_range = T, split_ABC = FALSE)
  relprobs_matrix = biogeo$ML_marginal_prob_each_state_at_branch_top_AT_node
  MLstates = get_ML_states_from_relprobs(relprobs_matrix, statenames, returnwhat = "states", if_ties = "takefirst")
  Ntip(tree) -> n.tips
  Nnode(tree) -> n.nodes
  c((n.tips++1):(n.tips++n.nodes)) -> nodes
  MLstates[nodes] -> node.states
  
  ### pies 
  
  unlist(statenames) -> pie.colors
  for (i in 1:length(pie.colors)) {
    pie.colors[i] -> col0
    nchar(col0) -> nn
    if(nn > 1) {
      col0 <- range.col
    } else {
      match(col0, areas.labs) -> nnn
      col0 <- areas.cols[nnn]
    }
    pie.colors[i] <- col0
  }
  
  pie.data <- relprobs_matrix[nodes,]
  colnames(pie.data) <- unlist(statenames)
  legend.col <- c(areas.cols, range.col)
  legend.txt <- c(areas.labels, "Widespread")
  
  out <- list(pie.data=pie.data, pie.colors=pie.colors, node.states=node.states, tip.states=tipStates, legend.txt=legend.txt, legend.col=legend.col)
  return(out)
}
