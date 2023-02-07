
source("./HGraph.R")
#
#datastruct

#input: a<-lavaan::sem, a[which(fttable$op=='~'),c('lhs', 'rhs')]
#output: edge of x2y
GetMatriOfPath <- function(x2yinfo, data_info) {
  lhs       <- x2yinfo['lhs'][[1]]
  rhs       <- x2yinfo['rhs'][[1]]
  union_var <- union(lhs, rhs)
  n_var     <- length(union_var)
  n_edge    <- length(lhs)
  dfm <- matrix(0, n_var, n_var, dimnames = list(union_var, union_var))
  for (i in 1:n_edge) {
    dfm[rhs[i], lhs[i]] <- 1
  }

  data_info <- append(data_info, list(edge=dfm, lhs_var=lhs, rhs_var=rhs, union_var=union_var, n_var=n_var))
  return (data_info)
}

##input  : lavvan_class_info
##output : which is x_var mid_var y_var
GetVarOfPropertyV2 <- function(lavvan_class_info, data_info) {
  x_set   <- lavaanNames(lavvan_class_info,type="eqs.x")
  y_set   <- lavaanNames(lavvan_class_info,type="eqs.y")
  mid_var <- intersect(x_set, y_set)
  x_var   <- setdiff(x_set, y_set)
  y_var   <- setdiff(y_set, x_set)
  data_info <- append(data_info, list(x_var=x_var, y_var=y_var, mid_var=mid_var))
  return (data_info)
}

GetDataInfo <- function(lavvan_class_info) {
  # get edge 
  myParTable <- parTable(lavvan_class_info)
  x2yinfo <- myParTable[which(myParTable$op == '~'), c('lhs', 'rhs')]
  ainfo <- list(ts=as.numeric(as.POSIXct(Sys.time())))
  ainfo <- GetMatriOfPath(x2yinfo, ainfo)
  ainfo <- GetVarOfPropertyV2(lavvan_class_info, ainfo)
  
  var_graph_info <- HGraphClass()
  var_graph_info$init(ainfo$edge, ainfo$union_var)
  var_graph_info$CreateGraph()
  ainfo <- append(ainfo, list(var_graph_info=var_graph_info))
  #y4 <- append(x1,x2,after = 2)
  return (ainfo)
}
