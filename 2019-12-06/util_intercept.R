library(MplusAutomation)
Mplus_command <- Mplus_CMD
datapath <- INPUTDATA
curpath <- SCRIPT_PATH
setwd(curpath)

#确定各个区间的个数
loadingnum <- Questionnum
interactionnum <- getFactorCorr()
factormeannum <- 0
if(HasFactormean > 0){
  factormeannum <- Factornum
}
interceptnum <- 0
if(HasInterception > 0){
  interceptnum <- Questionnum
}
varinum <- Factornum
residualnum <- Questionnum

getdiff1st <- function(fit1) {
  t <- fit1$results$parameters$unstandardized$est
  curlen <- length(t)
  cursize <- curlen / NGroup1st
  lst1 <- t[1:cursize]
  lst2 <- t[(cursize+1):(curlen)]
  diff1 <- lst1 - lst2
  ratio1 <- lst1 / lst2
  return (list(diff1,ratio1,lst1,lst2))
}
getdiff1st_v2 <- function(fit1) {
  t <- fit1$parameters$unstandardized$est
  curlen <- length(t)
  cursize <- curlen / NGroup1st
  lst1 <- t[1:cursize]
  lst2 <- t[(cursize+1):(curlen)]
  diff1 <- lst1 - lst2
  ratio1 <- lst1 / lst2
  return (list(diff1,ratio1,lst1,lst2))
}

dodiff <- function(fit1){
  estlst <- fit1$results$parameters$unstandardized$est
  beginidx <- 0
  loadlst <- estlst[(beginidx+1):loadingnum]
  beginidx <- beginidx+loadingnum
  
  interactionlst <- NULL
  if(interactionnum > 0){
    interactionlst <- estlst[(beginidx+1):(beginidx+interactionnum)]
  }
  beginidx <- beginidx+interactionnum
  
  factormeanlst <- NULL
  if(factormeannum > 0){
    factormeanlst <- estlst[(beginidx+1):(beginidx+factormeannum)]
  }
  beginidx <- beginidx+factormeannum
  
  interceptlst <- NULL
  if(interceptnum > 0){ 
    interceptlst <- estlst[(beginidx+1):(beginidx+interceptnum)]
  }
  beginidx <- beginidx+interceptnum
  
  varilst <- estlst[(beginidx+1):(beginidx+varinum)]
  beginidx <- beginidx+varinum
  
  residlst <- estlst[(beginidx+1):(beginidx+residualnum)]
  beginidx <- beginidx+residualnum
  
  loadlst2 <- estlst[(beginidx+1):(beginidx+loadingnum)]
  beginidx <- beginidx+loadingnum
  
  interactionlst2 <- NULL
  if(interactionnum > 0){
    interactionlst2 <- estlst[(beginidx+1):(beginidx+interactionnum)]
  }
  beginidx <- beginidx+interactionnum
  
  factormeanlst2 <- NULL
  if(factormeannum > 0){
    factormeanlst2 <- estlst[(beginidx+1):(beginidx+factormeannum)]
  }
  beginidx <- beginidx+factormeannum
  
  interceptlst2 <- NULL
  if(interceptnum > 0){ 
    interceptlst2 <- estlst[(beginidx+1):(beginidx+interceptnum)]
  }
  beginidx <- beginidx+interceptnum
  
  varilst2 <- estlst[(beginidx+1):(beginidx+varinum)]
  beginidx <- beginidx+varinum
  
  residlst2 <- estlst[(beginidx+1):(beginidx+residualnum)]
  beginidx <- beginidx+residualnum
  
  diffintercept <- interceptlst - interceptlst2
}

dodiffbyparam1st <- function(fit1, tloadingnum, tinteractionnum, tfactormeannum, tinterceptnum, tvarinum, tresidualnum){
  estlst <- fit1$results$parameters$unstandardized$est
  beginidx <- 0
  loadlst <- estlst[(beginidx+1):tloadingnum]
  beginidx <- beginidx+tloadingnum
  
  interactionlst <- NULL
  if(tinteractionnum > 0){
    interactionlst <- estlst[(beginidx+1):(beginidx+tinteractionnum)]
  }
  beginidx <- beginidx+tinteractionnum
  
  factormeanlst <- NULL
  if(tfactormeannum > 0){
    factormeanlst <- estlst[(beginidx+1):(beginidx+tfactormeannum)]
  }
  beginidx <- beginidx+tfactormeannum
  
  interceptlst <- NULL
  if(tinterceptnum > 0){ 
    interceptlst <- estlst[(beginidx+1):(beginidx+tinterceptnum)]
  }
  beginidx <- beginidx+tinterceptnum
  
  varilst <- estlst[(beginidx+1):(beginidx+tvarinum)]
  beginidx <- beginidx+tvarinum
  
  residlst <- estlst[(beginidx+1):(beginidx+tresidualnum)]
  beginidx <- beginidx+tresidualnum
  
  loadlst2 <- estlst[(beginidx+1):(beginidx+tloadingnum)]
  beginidx <- beginidx+tloadingnum
  
  interactionlst2 <- NULL
  if(interactionnum > 0){
    interactionlst2 <- estlst[(beginidx+1):(beginidx+tinteractionnum)]
  }
  beginidx <- beginidx+tinteractionnum
  
  factormeanlst2 <- NULL
  if(tfactormeannum > 0){
    factormeanlst2 <- estlst[(beginidx+1):(beginidx+tfactormeannum)]
  }
  beginidx <- beginidx+tfactormeannum
  
  interceptlst2 <- NULL
  if(tinterceptnum > 0){ 
    interceptlst2 <- estlst[(beginidx+1):(beginidx+tinterceptnum)]
  }
  beginidx <- beginidx+tinterceptnum
  
  varilst2 <- estlst[(beginidx+1):(beginidx+tvarinum)]
  beginidx <- beginidx+tvarinum
  
  residlst2 <- estlst[(beginidx+1):(beginidx+tresidualnum)]
  beginidx <- beginidx+tresidualnum
  
  return (list(loadlst, interactionlst, factormeanlst, interceptlst, varilst, residlst, loadlst2, interactionlst2, factormeanlst2, interceptlst2, varilst2, residlst2))
}



###########################################################################
#下面这个是辅助函数，辅助生成选择intercept referent的模型中截距估计的部分 #
###########################################################################
selectreferent_perfactor_helper <- function(begin1, end1, fixinterceptidx){
  ret <- ""
  template1 <- "[V%d-V%d];"
  template2 <- "[V%d];[V%d-V%d];"
  template3 <- "[V%d-V%d];[V%d];"
  template4 <- "[V%d-V%d];[V%d-V%d];"
  if (begin1 == fixinterceptidx) {
    ret <- sprintf(template1, begin1+1, end1)
  } else if(end1 == fixinterceptidx) {
    ret <- sprintf(template1, begin1, end1-1)
  } else if(begin1 == fixinterceptidx-1){
    ret <- sprintf(template2, begin1, fixinterceptidx+1, end1)
  } else if(end1 == fixinterceptidx+1){
    ret <- sprintf(template3, begin1, fixinterceptidx-1, end1)
  } else {
    ret <- sprintf(template4, begin1, fixinterceptidx-1, fixinterceptidx+1, end1)
  }
  return (ret)
}
#######################################################################################################
# 下面的函数用于生成模型主体结构，该模型用于进行intercept referent的选择，在选择时，拟每个因子单独选择#
# 函数参数为题目数量、因子个数、寻找第几个因子上的referent、固定截距相等的题号（该因子的第几道题）    #
#######################################################################################################
selectreferent_perfactor<-function(cQuesNum, cFactorNum, factoridx, interceptidx){
  usevarbegin <- ((cQuesNum/cFactorNum)*(factoridx-1)+1)
  usevarend <- ((cQuesNum/cFactorNum)*factoridx)
  
  retmodel <- ""
  template1 <- "
  NAMES = V1-V%d;
  USEVARIABLES = V%d-V%d;
  grouping=V%d(1=g1 2=g2);
  "
  model_VARIABLE <- sprintf(template1, (cQuesNum+1), usevarbegin, usevarend, (cQuesNum+1))
  
  template2 <- "
  f%d by V%d-V%d;
  model g1:
  f%d by V%d-V%d(L%d-L%d);
  f%d@1;
  [f%d];
  [V%d@0];
  %s
  V%d-V%d;
  model g2:
  f%d by V%d-V%d(L%d-L%d);
  f%d;
  [f%d];
  [V%d@0];
  %s
  V%d-V%d;
  "
  
  freeintercept <- selectreferent_perfactor_helper(usevarbegin, usevarend, interceptidx+usevarbegin-1)
  model_MODEL <- sprintf(template2, 
                         factoridx, usevarbegin, usevarend, #model
                         factoridx, usevarbegin, usevarend, usevarbegin, usevarend,#model g1
                         factoridx,
                         factoridx,
                         interceptidx+usevarbegin-1,
                         freeintercept,
                         usevarbegin, usevarend,
                         factoridx, usevarbegin, usevarend, usevarbegin, usevarend,#model g2
                         factoridx,
                         factoridx,
                         interceptidx+usevarbegin-1,
                         freeintercept,
                         usevarbegin, usevarend)
  return (list(model_VARIABLE, model_MODEL))
}
selectreferent_perfactor_multi_helper <- function(cQuesNum, cFactorNum, interceptidxlst){
  template1 <- "[V%d@0];\n"
  ret <- ""
  len1 <- as.integer(cQuesNum/cFactorNum)
  for(i in 1:cFactorNum){
    delidx <- interceptidxlst[i]
    begin1 <- ((i-1)*len1+1)
    end1 <- len1*i
    c1 <- sprintf(template1, delidx+begin1-1)
    str1 <- selectreferent_perfactor_helper(begin1, end1, delidx+begin1-1)
    ret <- paste0(ret,c1)
    ret <- paste0(ret,str1)
  }
  return (ret)
}
getModelHeader<-function(cQuesNum, cFactorNum){
  len1 <- as.integer(cQuesNum/cFactorNum)
  ret <- ""
  ret2 <- ""
  template <- "f%d by V%d-V%d;\n"
  template2 <- "f%d by V%d-V%d(L%d-L%d);\n"
  for(i in 1:cFactorNum){
    cur <- sprintf(template, i, ((i-1)*len1+1), (i*len1))
    ret <- paste0(ret,cur)
    cur2 <- sprintf(template2, i, ((i-1)*len1+1), (i*len1), ((i-1)*len1+1), (i*len1))
    ret2 <- paste0(ret2,cur2)
  }
  return (list(ret,ret2))
}

selectreferent_perfactor_multifactor<-function(cQuesNum, cFactorNum, interceptidxlst){
  retmodel <- ""
  template1 <- "
  NAMES = V1-V%d;
  USEVARIABLES = V1-V%d;
  grouping=V%d(1=g1 2=g2);
  "
  model_VARIABLE <- sprintf(template1, (cQuesNum+1), cQuesNum, (cQuesNum+1))
  
  model_list <- getModelHeader(cQuesNum, cFactorNum)
  model_header <- unlist(model_list[1])
  model_header2 <- unlist(model_list[2])
  
  template2 <- "
  %s
  model g1:
  %s
  f1-f%d@1;
  [f1-f%d];
  %s
  V1-V%d;
  model g2:
  %s
  f1-f%d;
  [f1-f%d];
  %s
  V1-V%d;
  "
  model_header3 <- selectreferent_perfactor_multi_helper(cQuesNum, cFactorNum, interceptidxlst)
  model_MODEL <- sprintf(template2, 
                         model_header,
                         model_header2,
                         cFactorNum,
                         cFactorNum,
                         model_header3,
                         cQuesNum,
                         model_header2,
                         cFactorNum,
                         cFactorNum,
                         model_header3,
                         cQuesNum)
  
  return (list(model_VARIABLE, model_MODEL))
}

selectreferent_perfactor_merge_0<-function(cQuesNum, cFactorNum, interceptidxlst){
  
  retmodel <- ""
  template1 <- "
  NAMES = V1-V%d;
  USEVARIABLES = V1-V%d;
  "
  model_VARIABLE <- sprintf(template1, (cQuesNum+1), cQuesNum)
  
  ###f1 by V1* V2-V5;
  template1_1 <- "f%d by V%d* V%d-V%d;\n"
  len1 <- as.integer(cQuesNum/cFactorNum)
  str1_1 <- ""
  for(i in 1:cFactorNum){
    begin1 <- ((i-1)*len1+1)
    end1 <- (i*len1)
    c1 <- sprintf(template1_1, i, begin1, (begin1+1), end1)
    str1_1 <- paste0(str1_1, c1)
  }
  
  template2_2 <- "
  f1-f%d@1;
  [f1-f%d];"
  str2_2 <- sprintf(template2_2, cFactorNum, cFactorNum)
  if(cFactorNum == 1){
    str2_2 <- "
    f1@1;
    [f1];
    "
  }
  
  str3_3 <- selectreferent_perfactor_multi_helper(cQuesNum, cFactorNum, interceptidxlst)
  
  template4_4 <- "
  %s
  %s
  %s
  V1-V%d;
  "
  model_MODEL <- sprintf(template4_4, 
                         str1_1, 
                         str2_2,
                         str3_3,
                         cQuesNum)
  
  return (list(model_VARIABLE, model_MODEL))
}

##这是一个BIG Project，函数命名尽可能保证唯一性，Thx
sepAndMergeEst1st <- function(lst1, fname, fixidxlst, cfactornum, resfile, model_var, model_model, rstep) {
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  t1 <- ""
  t1 <- paste0(t1,cfactornum)
  for(i in 1:cfactornum){
    t1 <- paste0(t1,"_",fixidxlst[i])
  }
  retfile1 <- paste0(fname,"_",t1,".inp")
  if(rstep == 2){
    retfile1 <- paste0(fname,".inp")
  }
  
  ##Mplus 运行的参数配置
  ##COMMENT BY LiHui
  pathmodel <- mplusObject(
    TITLE = "this model is used to select intercept seperateEst1st referent;",
    VARIABLE = model_var,
    MODEL = model_model,
    OUTPUT = "stdyx tech1;",
    rdata = lst1,)
  
  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
  return (fit)
}
