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

###########################################################################
#下面这个是辅助函数，辅助生成选择intercept referent的模型中截距估计的部分 #
###########################################################################
#######################################################################################################
# 下面的函数用于生成模型主体结构，该模型用于进行intercept referent的选择，在选择时，拟每个因子单独选择#
# 函数参数为题目数量、因子个数、寻找第几个因子上的referent、固定截距相等的题号（该因子的第几道题）    #
#######################################################################################################

getModelHeader<-function(cQuesNum, cFactorNum){
  len1 <- as.integer(cQuesNum/cFactorNum)
  ret <- ""
  ret2 <- ""
  template <- "f%d by V%d-V%d;\n"
  template2 <- "f%d by V%d@1;\n"
  for(i in 1:cFactorNum){
    cur <- sprintf(template, i, ((i-1)*len1+1), (i*len1))
    ret <- paste0(ret,cur)
    cur2 <- sprintf(template2, i, ((i-1)*len1+1))
    ret2 <- paste0(ret2,cur2)
  }
  return (paste0(ret,ret2))
}

getModelHeaderA<-function(cQuesNum, cFactorNum){
  len1 <- as.integer(cQuesNum/cFactorNum)
  ret <- ""
  template <- "f%d by V%d-V%d(A%d-A%d);\n"
  for(i in 1:cFactorNum){
    cur <- sprintf(template, i, ((i-1)*len1+2), (i*len1), ((i-1)*len1+2), (i*len1))
    ret <- paste0(ret,cur)
  }
  template2 <- "f1%s;\n V1-V%d;\n"
  ret2 <- ""
  if(cFactorNum > 1){
    ret2 <- paste0("-f",cFactorNum)
  }
  ret3 <- sprintf(template2, ret2, cQuesNum)
  return (paste0(ret,ret3))
}

getModelHeaderB<-function(cQuesNum, cFactorNum){
  len1 <- as.integer(cQuesNum/cFactorNum)
  ret <- ""
  template <- "f%d by V%d-V%d(B%d-B%d);\n"
  for(i in 1:cFactorNum){
    cur <- sprintf(template, i, ((i-1)*len1+2), (i*len1), ((i-1)*len1+2), (i*len1))
    ret <- paste0(ret,cur)
  }
  template2 <- "f1%s;\n V1-V%d;\n"
  ret2 <- ""
  if(cFactorNum > 1){
    ret2 <- paste0("-f",cFactorNum)
  }
  ret3 <- sprintf(template2, ret2, cQuesNum)
  return (paste0(ret,ret3))
}

selectv1<-function(cQuesNum, cFactorNum){
  retmodel <- ""
  template1 <- "
  NAMES = V1-V%d;
  USEVARIABLES = V1-V%d;
  grouping=V%d(1=g1 2=g2);
  "
  model_VARIABLE <- sprintf(template1, (cQuesNum+1), cQuesNum, (cQuesNum+1))
  
  model_header <- getModelHeader(cQuesNum, cFactorNum)
  model_headerA <- getModelHeaderA(cQuesNum, cFactorNum)
  model_headerB <- getModelHeaderB(cQuesNum, cFactorNum)
  
  template2 <- "
  %s
  model g1:
  %s
  model g2:
  %s
  "
  model_MODEL <- sprintf(template2, 
                         model_header,
                         model_headerA,
                         model_headerB)  

  return (list(model_VARIABLE, model_MODEL))
}

selectv2<-function(cQuesNum, cFactorNum){
  retmodel <- ""
  template1 <- "
  NAMES = V1-V%d;
  USEVARIABLES = V1-V%d;
  grouping=V%d(1=g1 2=g2);
  "
  model_VARIABLE <- sprintf(template1, (cQuesNum+1), cQuesNum, (cQuesNum+1))
  
  model_header <- getModelHeader(cQuesNum, cFactorNum)
  model_headerA <- getModelHeaderA(cQuesNum, cFactorNum)
  
  template2 <- "
  %s
  model g1:
  %s
  model g2:
  %s
  "
  model_MODEL <- sprintf(template2, 
                         model_header,
                         model_headerA,
                         model_headerA)  

  return (list(model_VARIABLE, model_MODEL))
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
########################




########################
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

estECC <- function(lst1, fname, resfile, model_var, model_model) {
  library(MplusAutomation)
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(fname,".inp")

  ##Mplus 运行的参数配置
  ##COMMENT BY LiHui
  pathmodel <- mplusObject(
    TITLE = "this model is used to seperate estimate;",
    DATA = "LISTWISE = ON;",
    VARIABLE = model_var,
    ANALYSIS = "TYPE = GENERAL; MODEL = NOMEANSTRUCTURE;INFORMATION = EXPECTED;",
    MODEL = model_model,
    OUTPUT = "stdyx tech1;",
    rdata = lst1)

  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
  return (fit)
}

estECCCON <- function(lst1, fname, resfile, model_var, model_model, testitem) {
  library(MplusAutomation)
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(fname,"_",testitem,".inp")

  templates <- "0=A%d-B%d;\n"
  model_constraint <- sprintf(templates, testitem, testitem)

  ##Mplus 运行的参数配置
  ##COMMENT BY LiHui
  pathmodel <- mplusObject(
    TITLE = "this model is used to seperate estimate;",
    DATA = "LISTWISE = ON;",
    VARIABLE = model_var,
    ANALYSIS = "TYPE = GENERAL; MODEL = NOMEANSTRUCTURE;INFORMATION = EXPECTED;",
    MODEL = model_model,
    MODELCONSTRAINT = model_constraint, 
    OUTPUT = "stdyx tech1;",
    rdata = lst1)

  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
  return (fit)
}


