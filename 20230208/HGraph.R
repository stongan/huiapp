
###https://rdrr.io/r/methods/refClass.html
###http://adv-r.had.co.nz/R5.html
###https://bookdown.org/xiao/RAnalysisBook/section-2.html#section-2.1.3

HGraphNode <- setRefClass("HGraphNode",
  fields = list(
    id       = "numeric", 
    name     = "character",
    parent   = "vector",
    children = "vector",
    isStart  = "logical",
    isEnd    = "logical"
  ),
  methods = list(
    initialize = function(){
      id       <<- 0
      name     <<- ""
      parent   <<- vector(mode = "character", length = 0)
      children <<- vector(mode = "character", length = 0)
      isStart  <<- FALSE
      isEnd    <<- FALSE
    }
  )
)

HGraphClass <- setRefClass("HGraphClass",
  fields=list(edgeMat   = "matrix",
              varList   = "vector",
              nVar      = "numeric",
              name2id   = "list",
              graphInfo = "list")
)
HGraphClass$methods(
  init = function(edgeMat_t, var_list_t) {
    edgeMat <<- edgeMat_t
    varList <<- var_list_t
    nVar    <<- length(var_list_t)
    name2id <<- setNames(as.list(rep(1:nVar)), varList)
    for (i in 1:nVar) {
      graphInfo[[i]] <<- HGraphNode()
    }
  }
)
HGraphClass$methods(
  CreateGraph = function() {
    for (i in 1:nVar) {
      graphInfo[[i]]$id   <<- i
      graphInfo[[i]]$name <<- varList[i]
      for (j in 1:nVar) {
        if (edgeMat[varList[i], varList[j]] == 1) {
          graphInfo[[i]]$children <<- c(graphInfo[[i]]$children, varList[j])
          graphInfo[[j]]$parent   <<- c(graphInfo[[j]]$parent, varList[i])
        }
      }
    }
    for (i in 1:nVar) {
      cur_node <- graphInfo[[i]]
      if (length(cur_node$parent) < 1) {
        graphInfo[[i]]$isStart <<- TRUE
      }
      if (length(cur_node$children) < 1) {
        graphInfo[[i]]$isEnd <<- TRUE
      }
    } #end for
  } #end function
)
  

DFS <- function(data_info, idx) {
  data_info$visited[idx] <- TRUE
  mid_graph_info <- data_info$mid_graph_info
  mid_name2id    <- data_info$mid_name2id
  visited        <- data_info$visited
  
  print (mid_graph_info[[idx]]$name)
  print (visited)
  if (mid_graph_info[[idx]]$isEnd) {
    print ("--------------------")
  }
  children <- mid_graph_info[[idx]]$children
  if (length(children) > 0) {
    for(i in 1:length(children)){
      children_name <- children[i]
      children_id   <- mid_name2id[[children_name]]
      if (!visited[children_id]) {
        DFS(data_info, children_id)
      }
    }
  }
  return (data_info)
}

TravelMidGraph <- function(lavvan_class_info, data_info) {
  mid_graph_info <- data_info$mid_graph_info
  mid_name2id    <- data_info$mid_name2id
  n_mid          <- length(mid_graph_info)
  visited <- c(rep(FALSE,n_mid))
  data_info <- append(data_info, list(visited=visited))
  print ("begin TravelMidGraph")
  for(i in 1:n_mid) {
    print ("outloop")
    print (data_info$visited)
    if (!data_info$visited[i]) {
      DFS(data_info, i)
    }
  }
  print ("end TravelMidGraph")
}