getSummaryBioGeoBEARS <- function(x, tree) {
  require(BioGeoBEARS)
  require(ape)
  require(paleotree)
  require(phangorn)
  
  x -> biogeo
  
  tipranges = getranges_from_LagrangePHYLIP(lgdata_fn=biogeo$inputs$geogfn)
  areas.labs = getareas_from_tipranges_object(tipranges)
  statenames = areas_list_to_states_list_new(areas.labs, maxareas = biogeo$inputs$max_range_size, include_null_range = T, split_ABC = FALSE)
  pie.data = biogeo$ML_marginal_prob_each_state_at_branch_top_AT_node
  colnames(pie.data) <- unlist(statenames)
  data.frame(pie.data[,-1]) -> pie.data
  round(pie.data,2) -> pie.data
  
  Ntip(tree) -> n.tips
  Nnode(tree) -> n.nodes
  c((n.tips++1):(n.tips++n.nodes)) -> nodes
  
  ### Get events
  
  suppressWarnings(dateNodes(tree)) -> nodes.ages
  round(nodes.ages, 2) -> nodes.ages
  data.frame(age=nodes.ages, row.names = names(nodes.ages)) -> all.events
  all.events$node <- "tip"
  all.events$node[nodes] <- "node"
  all.events$node[nodes[1]] <- "root"
  
  all.events$estimate <- NA
  all.events$prob <- NA
  all.events$ancestor <- NA
  all.events$age.ancestor <- NA
  all.events$desc.1 <- NA
  all.events$desc.2 <- NA
  
  for (i in 1:nrow(all.events)) {
    pie.data[i,] -> p0
    all.events[i,] -> n0
    ### estimate
    n0$estimate <- names(which.max(p0))
    n0$prob <- as.numeric(p0[n0$estimate])
    
    ### ancestor
    Ancestors(tree, as.numeric(rownames(n0)), type="parent") -> anc0
    if (anc0 > 0) {
      n0$age.ancestor <- nodes.ages[anc0]
      n0$ancestor <- names(which.max(pie.data[anc0,]))
    }
    ### descendants
    Descendants(tree, as.numeric(rownames(n0)), type="children") -> desc0
    if (length(desc0) > 0) {
      desc0[1] -> d0
      desc0[2] -> d1
      n0$desc.1 <- names(which.max(pie.data[d0,]))
      n0$desc.2 <- names(which.max(pie.data[d1,]))
    }
    all.events[i,] <- n0
  }

  ### Event type
  
  all.events$change <- "No"
  all.events$type <- ""
  all.events$description <- ""
  
  for (i in 1:nrow(all.events)) {
    all.events[i,] -> n0
    n0$ancestor -> anc0
    if (is.na(anc0)==F) {
      n0$estimate -> c0
      if (anc0 != c0) {
        n0$change <- "yes"
        sub("|", "", anc0, fixed=T) -> anc0
        sub(c0, "", anc0) -> diff0
        if (length(grep(c0, anc0)) == 0) {
          ### dispersal
          n0$type <- "dispersal"
          n0$description <- paste(anc0, "->", c0, sep="")
        } else {
          if (nchar(diff0) > 0) {
            ### subset
            n0$type <- "vicariance/subset"
            
          } else {
            ### dispersal
            sub("|", "", c0, fixed=T) -> c0
            sub(anc0, "", c0) -> diff1
            n0$type <- "dispersal"
            n0$description <- paste(anc0, "->", diff1, sep="")
          }
        }
      }
    }
    n0 -> all.events[i,]
  }
  
  all.events$type[which(all.events$change == "No")] <- "sympatry"
  all.events$change[nodes[1]] <- NA
  all.events$type[nodes[1]] <- NA
  
  
  ######################################
  ### Dispersal between areas
  ######################################
  
  all.events$description -> desc0
  strsplit(desc0, "->") -> desc0
  all.events$from <- unlist(lapply(desc0, "[", 1))
  all.events$to <- unlist(lapply(desc0, "[", 2))
  for (i in 1:nrow(all.events)) {
    if (is.na(all.events$from[i])==F) {
      sub(all.events$from[i], "", all.events$to[i]) -> all.events$to[i]
    }
  }
  return(all.events)
}
