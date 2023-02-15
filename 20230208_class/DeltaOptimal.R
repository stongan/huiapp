source("./DataInfo.R")

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
  if (length(ainfo$mid_var) > 0) {
    ainfo$mid_graph_info$Runner(ainfo$midEdgeInfo, ainfo$mid_var)
  }
  
  ainfo$AddDoubleWavy()
  ainfo$GetMidCalMatrix()
  ainfo$CalSimulateExoVarQuote()
  ainfo$CalMidCalMatrixQuoteList()
  ainfo$CalYHatEx()
  return (ainfo)
}

GetDeltaR2Core <- function(origin_data, model) {
  lavvan_class_info <- lavaan::sem(model, data = origin_data)
  
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
  if (length(ainfo$mid_var) > 0) {
    ainfo$mid_graph_info$Runner(ainfo$midEdgeInfo, ainfo$mid_var)
  }
  
  ainfo$AddDoubleWavy()
  ainfo$GetMidCalMatrix()
  ainfo$CalSimulateExoVarQuote()
  ainfo$CalMidCalMatrixQuoteList()
  ainfo$CalYHatEx()
  return (ainfo$delta_r2_matrix[, "delta_R2"])
}

GetDataInfoLinkM <- function(origin_data, model) {
  fit <- lavaan::sem(model, data = origin_data)
  dfm <- GetDataInfo(fit, origin_data)
  model2 <- paste0(model, dfm$toAddWavyStr)
  fit2 <- lavaan::sem(model2, data = origin_data)
  dfm2 <- GetDataInfo(fit2, origin_data)
  return (dfm2)
}

deltaR2 <- function(data, model, targetM="") {
  fit             <- lavaan::sem(model, data = data)
  dfm             <- GetDataInfo(fit, data)
  delta_r2_matrix <- dfm$delta_r2_matrix
  if (targetM %in% row.names(delta_r2_matrix)) {
    return (delta_r2_matrix[targetM, "delta_R2"])
  } else {
    return (delta_r2_matrix[, "delta_R2"])
  }
}

deltaR2.CI <- function(data, model, targetM="", conf=0.95, R=1000, type="perc") {
  delta_r_t  <- GetDeltaR2Core(data, model)
  delta_name <- names(delta_r_t)
  col_name   <- c("deltaR2.est", "conf.lower", "conf.upper", "replication")
  result_delta <- matrix(NA, length(delta_name), 4, dimnames = list(delta_name, col_name))
  boot.fun <- function(obsdata, indices) {
    data_boot <- obsdata[indices,]
    colnames(data_boot) <- colnames(obsdata)
    delta_r_t <- GetDeltaR2Core(data_boot, model)
    #print (delta_r_t)
    as.vector(delta_r_t)
  }
  
  typelist   <- c("norm", "basic", "stud", "perc", "bca", "all")
  selecttype <- c("perc")
  if (type %in% typelist) {
    selecttype <- c(type)
  } else {
    stop("deltaR2.CI para of type is no valid")
  }
  boot.res   <- boot::boot(data, boot.fun, R = R)
  #boot.res
  for (i in 1:length(delta_name)) {
    r_name    <- delta_name[i]
    CInewR2M1 <- boot::boot.ci(boot.res, conf, type = selecttype, index = i)
    result_delta[r_name, col_name[1]] <- delta_r_t[r_name]
    result_delta[r_name, col_name[2]] <- CInewR2M1$percent[4]
    result_delta[r_name, col_name[3]] <- CInewR2M1$percent[5]
    result_delta[r_name, col_name[4]] <- CInewR2M1$R
  }
  return (result_delta)
}
