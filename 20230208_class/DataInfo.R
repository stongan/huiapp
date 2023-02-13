source("./HDataMeta.R")
source("./HGraph.R")

HDataInfo <- setRefClass("HDataInfo", contains="HDataMeta")

##init function
##input  : lavvan_class_info, origin_data
HDataInfo$methods(
GetVarOfProperty = function(lavvan_class_info, origin_data) {
  ts              <<- as.numeric(as.POSIXct(Sys.time()))
  lavvanClassInfo <<- lavvan_class_info
  originData      <<- origin_data
  eqs_x_set       <<- lavaan::lavaanNames(lavvan_class_info,type="eqs.x")
  eqs_y_set       <<- lavaan::lavaanNames(lavvan_class_info,type="eqs.y")
  mid_var         <<- intersect(eqs_x_set, eqs_y_set)
  x_var           <<- setdiff(eqs_x_set, eqs_y_set)
  y_var           <<- setdiff(eqs_y_set, eqs_x_set)
  all_var         <<- union(eqs_y_set, eqs_x_set)
  x_mid_var       <<- union(x_var, mid_var)
  n_var           <<- length(all_var)
  var_graph_info  <<- HGraphClass()
  mid_graph_info  <<- HGraphClass()
  
  # get varDict is ov or iv
  var2ovlvDict   <<- setNames(as.list(rep("", n_var)), all_var)
  lv_set          <- lavaan::lavaanNames(lavvan_class_info,type="lv")
  ov_set          <- lavaan::lavaanNames(lavvan_class_info,type="ov")
  for (i in 1:length(ov_set)) {
    if (ov_set[i] %in% all_var) {
      var2ovlvDict[[ov_set[i]]] <<- "ov"
    }
  }
  for (i in 1:length(lv_set)) {
    if (lv_set[i] %in% all_var) {
      var2ovlvDict[[lv_set[i]]] <<- "lv"
    }
  }
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
  c_len <- length(x_mid_var)
  x_mid_covarInfo <<- matrix(NA, c_len, c_len, dimnames = list(x_mid_var, x_mid_var))
  for(i in 1:c_len) {
    for(j in 1:c_len) {
      if( !is.na(covarInfo[x_mid_var[i], x_mid_var[j]]) ){
        x_mid_covarInfo[x_mid_var[i], x_mid_var[j]] <<- covarInfo[x_mid_var[i], x_mid_var[j]]
      } # end if
    } # end for-inner
  } # end for-outer
})

HDataInfo$methods(
GetR2 = function(x2yinfo_r2) {
  t_lhs <- x2yinfo_r2['lhs'][[1]]
  t_rhs <- x2yinfo_r2['rhs'][[1]]
  t_est <- x2yinfo_r2['est'][[1]]
  c_len <- length(t_lhs)
  # !!! consider only one y_var !!!
  cur_y_var <- y_var[1]
  for (i in 1:c_len) {
    if ((t_lhs[i] == cur_y_var) && (t_rhs[i] == cur_y_var)) {
      y_r2 <<- t_est[i] 
      break
    }
  }
  cur_y_alpha <- covarInfo[cur_y_var, cur_y_var]
  y_cal_variance <<- cur_y_alpha / (1-y_r2)
  if (var2ovlvDict[[cur_y_var]] == 'lv') { # cur_y_var is lv
    y_get_variance <<- lavaan::lavInspect(lavvanClassInfo,"cov.lv")[cur_y_var, cur_y_var]
  } else { # cur_y_var is ov
    y_get_variance <<- lavaan::lavInspect(lavvanClassInfo,"implied")$conv[cur_y_var, cur_y_var]
  }
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

HDataInfo$methods(
GetSimulateExoVar = function() {
  cur_x_mid_covarInfo <- x_mid_covarInfo
  cur_x_mid_covarInfo[is.na(cur_x_mid_covarInfo)] <- 0
  # get eigen
  cur_eigen <- eigen(cur_x_mid_covarInfo)$values
  cur_n_sample <- length(originData[, 1])
  cur_x_mid_mean <- rep(0, length(x_mid_var))
  #print (cur_n_sample)
  #check if format then mvrnorm else print error and stop
  if(all(cur_eigen > 0)) {
    simulateExoVar <<- MASS::mvrnorm(n = cur_n_sample,
                         cur_x_mid_mean, cur_x_mid_covarInfo, empirical = T)
  } else {
    stop("x_mid_covarInfo is not a positive definite matrix")
  }
})

HDataInfo$methods(
GetMidCalMatrix = function() {
  # use mid_graph's BFS result to cal
  mid_travel <- mid_graph_info$traveBfsList
  n_mid <- length(mid_travel)
  n_row <- length(originData[, 1])
  mid_cal_matrix <<- matrix(0, n_row, n_mid, dimnames = list(rep(1:n_row), mid_travel))
  graphInfo <- var_graph_info$graphInfo
  name2id <- var_graph_info$name2id

  # cal each mid_var's M_matrix
  for(i in 1:n_mid) {
    cur_mid    <- mid_travel[i]
    cur_id     <- name2id[[cur_mid]]
    cur_node   <- graphInfo[[cur_id]]
    cur_parent <- cur_node$parent
    if (length(cur_parent) > 0) {
      for(j in 1:length(cur_parent)) {
        parent_name <- cur_parent[j]
        weight <- weighInfo[parent_name, cur_mid]
        if (parent_name %in% mid_travel) { # parent is mid_var
          mid_cal_matrix[, cur_mid] <<- mid_cal_matrix[, cur_mid] + weight * mid_cal_matrix[, parent_name]
        } else { # parent is x_var
          mid_cal_matrix[, cur_mid] <<- mid_cal_matrix[, cur_mid] + weight * simulateExoVar[, parent_name]
        }
      } # end for-inner
    } # end if
    mid_cal_matrix[, cur_mid] <<- mid_cal_matrix[, cur_mid] + simulateExoVar[, cur_mid]
  } # end for-outer
})

HDataInfo$methods(
CalSimulateExoVarQuote = function() {
  # accord to DFS chain cal each mid_var's simulateExoVarQuote
  # init
  simulateExoVarQuote <<- simulateExoVar
  ## DFS:
  ## chain1 : "dem60->(w1)->dem61->(w2)->dem63"
  ## chain2 : "dem62->(w3)->dem63"
  ## if start dem60_quote = dem60; dem62_quote = dem62
  ## else chain ∑ demi*wi
  ##     dem61_quote = dem61 + dem60 * w1
  ##     dem63_quote = dem63 + dem62 * w3 + (dem60*w1 + dem61) * w2
  ## simulateExoVarQuote = simulateExoVarQuote + cal_chain_matrix_1 * w3 + cal_chain_matrix_2 * w2
  cal_chain_matrix    <- simulateExoVar
  cal_chain_matrix[,] <- 0
  cur_travelList      <- mid_graph_info$travelList
  n_chain             <- length(cur_travelList)
  if (n_chain > 0) {
    for (i in 1:n_chain) {
      cal_chain_matrix[,] <- 0
      one_travelList      <- cur_travelList[[i]]
      if (length(one_travelList) > 1) {
        for (j in 2:length(one_travelList)) {
          pre_node   <- one_travelList[j-1]
          now_node   <- one_travelList[j]
          weight_p_2_n <- weighInfo[pre_node, now_node]
          cal_chain_matrix[, now_node] <- weight_p_2_n * (cal_chain_matrix[, pre_node] + simulateExoVar[, pre_node])
        }
      }
      simulateExoVarQuote <<- simulateExoVarQuote + cal_chain_matrix
    }
  } else {
    print ("INFO:[CalSimulateExoVarQuote]: mid_var only has one")
  }
})

HDataInfo$methods(
CalMidCalMatrixQuoteList = function() {
  # init
  mid_travel <- mid_graph_info$traveBfsList
  n_mid <- length(mid_travel)
  for(i in 1:n_mid) {
    mid_cal_matrixQuoteList[[mid_travel[i]]] <<- mid_cal_matrix
  }
  
  # effect after Mi 
  ## when interest dem60 cal the effect on dem61 and dem63
  ## DFS:
  ## chain1 : "ind60->(w0)->dem60->(w1)->dem61->(w2)->dem63"
  ## chain2 : "ind60->(w3)->dem62->(w4)->dem63"
  cur_travelList  <- var_graph_info$travelList
  n_chain         <- length(cur_travelList)
  t_cal_matrix    <- mid_cal_matrix
  t_cal_matrix[,] <- 0
  if (n_chain > 0) {
    for (i in 1:n_chain) {
      t_cal_matrix[,]    <- 0     # set as 0 matrix
      one_travelList     <- cur_travelList[[i]]
      x_name_chain       <- one_travelList[1]
      accumulateWeight   <- 1
      one_travelList_mid <- one_travelList[-length(one_travelList)][-1] # only has mid_var, delete x and y
      if (length(one_travelList_mid) > 2) { 
        for (j in 1:length(one_travelList_mid)) { # the last mid not effect other m , no need consider
          pre_node         <- x_name_chain
          if (j > 1) {
            pre_node       <- one_travelList_mid[j-1]
          }
          now_node         <- one_travelList_mid[j]
          weight_p_2_n     <- weighInfo[pre_node, now_node]
          accumulateWeight <- accumulateWeight * weight_p_2_n
          t_cal_matrix[, now_node] <- accumulateWeight * simulateExoVar[, x_name_chain]
        }
        # now_node effect next_node
        ## cal j value effect k value
        for (j in 1:(length(one_travelList_mid)-1)) {
          now_node         <- one_travelList_mid[j]
          for (k in (j+1):length(one_travelList_mid)) {
            next_node      <- one_travelList_mid[k]
            mid_cal_matrixQuoteList[[now_node]][, next_node] <<- mid_cal_matrixQuoteList[[now_node]][, next_node] - t_cal_matrix[, next_node]
          }
        }
      }
    }
  }
  
})

GetDataInfo <- function(lavvan_class_info, origin_data) {
  ainfo         <- HDataInfo()
  #myParTable <- lavaan::parTable(lavvan_class_info)
  myParTable    <- lavaan::parameterEstimates(lavvan_class_info, rsquare = TRUE)
  x2yinfo       <- myParTable[which(myParTable$op == '~'), c('lhs', 'rhs', 'est')]
  x2yinfo_covar <- myParTable[which(myParTable$op == '~~'), c('lhs', 'rhs', 'est')]
  x2yinfo_r2    <- myParTable[which(myParTable$op == 'r2'), c('lhs', 'rhs', 'est')]
  ainfo$GetVarOfProperty(lavvan_class_info, origin_data)
  ainfo$GetMatriOfPath(x2yinfo)
  ainfo$GetCovar(x2yinfo_covar)
  ainfo$GetR2(x2yinfo_r2)
  ainfo$GetSimulateExoVar()
  
  ainfo$var_graph_info$Runner(ainfo$edgeInfo, ainfo$all_var)
  if (length(ainfo$mid_var) > 1) {
    ainfo$mid_graph_info$Runner(ainfo$midEdgeInfo, ainfo$mid_var)
  }
  
  ainfo$AddDoubleWavy()
  ainfo$GetMidCalMatrix()
  ainfo$CalSimulateExoVarQuote()
  ainfo$CalMidCalMatrixQuoteList()
  return (ainfo)
}
