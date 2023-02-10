
source("./HGraph.R")
#
#datastruct

#input: a<-lavaan::sem, a[which(fttable$op=='~'),c('lhs', 'rhs')]
#output: edge of x2y
HDataInfo <- setRefClass("HDataInfo",
  fields = list(
    ts              = "numeric",
    eqs_x_set       = "vector", 
    eqs_y_set       = "vector",
    mid_var         = "vector",
    x_var           = "vector",
    y_var           = "vector",
    lhs             = "vector",
    rhs             = "vector",
    est             = "vector",
    all_var         = "vector",
    n_var           = "numeric",
    n_edge          = "numeric",
    midEdgeInfo     = "matrix",
    edgeInfo        = "matrix",
    weighInfo       = "matrix",
    covarInfo       = "matrix",
    y_r2            = "numeric",
    y_cal_variance  = "numeric",
    var_graph_info  = "ANY",
    mid_graph_info  = "ANY",
    toAddDoubleWavy = "list",
    toAddWavyStr    = "character"
))

##input  : lavvan_class_info
##output : which is x_var mid_var y_var
HDataInfo$methods(
GetVarOfProperty = function(lavvan_class_info) {
  ts             <<- as.numeric(as.POSIXct(Sys.time()))
  eqs_x_set      <<- lavaan::lavaanNames(lavvan_class_info,type="eqs.x")
  eqs_y_set      <<- lavaan::lavaanNames(lavvan_class_info,type="eqs.y")
  mid_var        <<- intersect(eqs_x_set, eqs_y_set)
  x_var          <<- setdiff(eqs_x_set, eqs_y_set)
  y_var          <<- setdiff(eqs_y_set, eqs_x_set)
  all_var        <<- union(eqs_y_set, eqs_x_set)
  n_var          <<- length(all_var)
  var_graph_info <<- HGraphClass()
  mid_graph_info <<- HGraphClass()
})

HDataInfo$methods(
GetMatriOfPath = function(x2yinfo) {
  lhs       <<- x2yinfo['lhs'][[1]]
  rhs       <<- x2yinfo['rhs'][[1]]
  est       <<- x2yinfo['est'][[1]]
  n_edge    <<- length(lhs)
  edgeInfo  <<- matrix(0, n_var, n_var, dimnames = list(all_var, all_var))
  weighInfo <<- matrix(NA, n_var, n_var, dimnames = list(all_var, all_var))
  c_edge    <-  0
  for (i in 1:n_edge) {
    if ((rhs[i] %in% eqs_x_set) && (lhs[i] %in% eqs_y_set)) {
      edgeInfo[rhs[i], lhs[i]] <<- 1
      weighInfo[rhs[i], lhs[i]] <<- est[i]
      c_edge <- c_edge + 1
    }
  } # end for
  n_edge <<- c_edge ## doctor Li say is necessary
  
  # get mid_var edgeinfo
  n_mid <- length(mid_var)
  midEdgeInfo <<- matrix(0, n_mid, n_mid, dimnames = list(mid_var, mid_var))
  if (n_mid > 1) {
    for (i in 1:n_mid) {
      for(j in 1:n_mid) {
        if ((i!=j) && (edgeInfo[mid_var[i], mid_var[j]] == 1)) {
          midEdgeInfo[mid_var[i], mid_var[j]] <<- 1
        }
      } # end for-inner
    } # end for-outer
  } # end n_mid > 1
})

HDataInfo$methods(
GetCovar = function(x2yinfo_covar) {
  t_lhs <- x2yinfo_covar['lhs'][[1]]
  t_rhs <- x2yinfo_covar['rhs'][[1]]
  t_est <- x2yinfo_covar['est'][[1]]
  c_edge <- length(t_lhs)
  covarInfo <<- matrix(NA, n_var, n_var, dimnames = list(all_var, all_var))
  for (i in 1:c_edge) {
    if ((t_rhs[i] %in% all_var) && (t_lhs[i] %in% all_var)) {
      covarInfo[t_rhs[i], t_lhs[i]] <<- t_est[i]
    }
  } # end for
})

HDataInfo$methods(
GetR2 = function(x2yinfo_r2) {
  t_lhs <- x2yinfo_r2['lhs'][[1]]
  t_rhs <- x2yinfo_r2['rhs'][[1]]
  t_est <- x2yinfo_r2['est'][[1]]
  c_len <- length(t_lhs)
  # consider only one y_var
  cur_y_var <- y_var[1]
  for (i in 1:c_len) {
    if ((t_lhs[i] == cur_y_var) && (t_rhs[i] == cur_y_var)) {
      y_r2 <<- t_est[i] 
      break
    }
  }
  cur_y_alpha <- covarInfo[cur_y_var, cur_y_var]
  y_cal_variance <<- cur_y_alpha / (1-y_r2)
})

HDataInfo$methods(
AddDoubleWavy = function() {
  mid_reachable   <- mid_graph_info$reachable
  toAddDoubleWavy <<- list()
  toAddWavyStr    <<- "\n"
  n_mid           <- length(mid_var)
  idx <- 1
  if (n_mid > 1) {
    for (i in 1:n_mid) {
      for(j in i:n_mid) {
        if ((i<j) &&  ## not in a chain and not hava covarinfo
            (is.na(covarInfo[mid_var[i], mid_var[j]])) &&
            (is.na(covarInfo[mid_var[j], mid_var[i]])) &&
            (mid_reachable[mid_var[i], mid_var[j]] != 1) &&
            (mid_reachable[mid_var[j], mid_var[i]] != 1)
        ) {
          toAddDoubleWavy[[idx]] <<- c(mid_var[i], mid_var[j])
          toAddWavyStr <<- paste0(toAddWavyStr, mid_var[i], "~~",mid_var[j], "\n")
          idx <- idx + 1
        }
      } # end for-inner
    } # end for-outer
  } # end n_mid > 1
})

GetDataInfo <- function(lavvan_class_info) {
  ainfo         <- HDataInfo()
  #myParTable <- lavaan::parTable(lavvan_class_info)
  myParTable    <- lavaan::parameterEstimates(lavvan_class_info, rsquare = TRUE)
  x2yinfo       <- myParTable[which(myParTable$op == '~'), c('lhs', 'rhs', 'est')]
  x2yinfo_covar <- myParTable[which(myParTable$op == '~~'), c('lhs', 'rhs', 'est')]
  x2yinfo_r2    <- myParTable[which(myParTable$op == 'r2'), c('lhs', 'rhs', 'est')]
  ainfo$GetVarOfProperty(lavvan_class_info)
  ainfo$GetMatriOfPath(x2yinfo)
  ainfo$GetCovar(x2yinfo_covar)
  ainfo$GetR2(x2yinfo_r2)
  
  ainfo$var_graph_info$Runner(ainfo$edgeInfo, ainfo$all_var)
  if (length(ainfo$mid_var) > 1) {
    ainfo$mid_graph_info$Runner(ainfo$midEdgeInfo, ainfo$mid_var)
  }
  
  ainfo$AddDoubleWavy()
  return (ainfo)
}
