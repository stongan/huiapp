#' @title Get travel path of graph

#' @description Get travel path of graph

#' @param edgeMatrix varVec

#' @return Graph struct

#' @examples get_graph(edgeMatrix,varVec)

#' @export get_graph_info
vec <- c("a", "b", "c")
edge <- matrix(0, 3, 3, dimnames=list(vec, vec))
edge["a","b"]<-1
edge["a","c"]<-1
edge["b","c"]<-1
get_graph_info <- function(edgeMatrix=edge, varVec=vec) {
  ginfo <- HGraphClass()
  ginfo$Runner(edgeMatrix, varVec)
  return (ginfo)
}
