library(MplusAutomation)
library(foreach)
library(doSNOW)

Mplus_command <- Mplus_CMD
datapath <- INPUTDATA
curpath <- SCRIPT_PATH
setwd(curpath)

source("util_intercept_traditional.R")
njobs <- NJOBS
setDefaultClusterOptions(master="localhost")
clusterobj <- makeSOCKcluster(njobs)
registerDoSNOW(clusterobj)


#建文件夹
outMCFA <- paste0(curpath,"/estMCFA")
out0<-paste0(outMCFA,"/","configural_scale_loading")
out1<-paste0(outMCFA,"/","metric_scale_loading")
out2<-paste0(outMCFA,"/","item_loading")
if (!file.exists(outMCFA)){ dir.create(outMCFA, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(out0)){ dir.create(out0, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(out1)){ dir.create(out1, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(out2)){ dir.create(out2, recursive=TRUE, showWarnings=FALSE) }

factor_minlst <- rep(0, Factornum)
for(factoridx in 1:Factornum){
  factor_minlst[factoridx] <- 1
}

##读入数据list进行遍历文件
tfname <- read.table(paste0(datapath,INPUTDATA_LIST))
tmatrix <- as.matrix(tfname)

configural_scale_est<-function(outpath, cur2, f2) {
  curselectret_sep <- selectv1(Questionnum, Factornum) 
  model_var <- unlist(curselectret_sep[1])
  model_model <- unlist(curselectret_sep[2])
  curfit <- estECC(cur2, f2, outpath, model_var, model_model)  
}

metric_scale_est<-function(outpath, cur2, f2) {
  curselectret_sep <- selectv2(Questionnum, Factornum) 
  model_var <- unlist(curselectret_sep[1])
  model_model <- unlist(curselectret_sep[2])
  curfit <- estECC(cur2, f2, outpath, model_var, model_model)  
}

metric_item_est<-function(outpath, cur2, f2, testitem) {
  curselectret_sep <- selectv1(Questionnum, Factornum) 
  model_var <- unlist(curselectret_sep[1])
  model_model <- unlist(curselectret_sep[2])
  curfit <- estECCCON(cur2, f2, outpath, model_var, model_model, testitem)  
}

#主逻辑
funclst <- c("NREPS","loadingnum","factormeannum",
             "interactionnum","interceptnum","varinum","residualnum",
             "Questionnum","Factornum","HasInterception","HasFactormean",
             "NGroup1st","Group1_samplesize","Group2_samplesize","getFactorCorr",
             "getModelHeader", "getModelHeaderA", "getModelHeaderB", 
             "selectv2", "selectv1","estECCCON","estECC")
##遍历路径下的每一个.out文件
##for(r in 1:nrow(tmatrix)){
foreach(r=1:nrow(tmatrix), .verbose=T, .export=funclst, .combine="c") %dopar% {   
  ##为了确保每个model输出到对应的文件夹，每次循环后重置路径
  setwd(curpath)
  ##得到每一个文件名
  filename <- tmatrix[r]
  ##拼接文件路径得到文件的具体路径
  fileone <- paste0(datapath,filename)
  ##读文件并转成data.frame,可能是冗余的，读进来就是data.frame.
  cur1 <- read.table(fileone)
  cur2 <- data.frame(cur1)
  filetmpgroup <- unlist(strsplit(filename, split="\\."))
  f2 <- filetmpgroup[1]
  configural_scale_est(out0, cur2, f2)
  metric_scale_est(out1, cur2, f2)
  for(factoridx in 1:Factornum){
    for(fct2 in 1:(Questionnum/Factornum)){
      qn <- ((Questionnum/Factornum)*(factoridx-1)+fct2) 
      if (fct2 == factor_minlst[factoridx]){
        next
      } else {
        metric_item_est(out2, cur2, f2, qn)
      }
    }
  }
} #end for

test1 <- function(){
  fitscale <- readModels(out0, what="all")
  for(i in 1:length(names(fitscale))){
    ##之后的函数调用设计改变路径，每个循环初始化路径
    ##取当前文件名字
    curname <- names(fitscale)[i]
    ##切割文件名字，只要后缀名之前的一段
    cur1 <- unlist(strsplit(curname, split="\\."))
    ##抽每个out文件的参数
    fitlst <- fitscale[[curname]]$summaries
    ftmp <- t(fitlst)
    break
  }
}

scalefitresult<-function(outpath)
{
  fitscale <- readModels(outpath, what="all")
  fitlen<-length(fitscale[[1]]$summaries)
  rowname1 <- names(fitscale[[1]]$summaries[1,])
  cname <- rep("NA",length(names(fitscale)))
  allretC <- data.frame(matrix(NA,fitlen,0))
  row.names(allretC) <- rowname1

  for(i in 1:length(names(fitscale))){
    ##之后的函数调用设计改变路径，每个循环初始化路径
    ##取当前文件名字
    curname <- names(fitscale)[i]
    ##切割文件名字，只要后缀名之前的一段
    cur1 <- unlist(strsplit(curname, split="\\."))
    cname[i] <- cur1[length(cur1)-1]
    ##抽每个out文件的参数
    fitlst <- fitscale[[curname]]$summaries
    ##按文件名保存到矩阵，生成configural和metric model的拟合指数表格
    allretC[cname[i]] <- t(fitlst)
  }

  #提取数值列并生成差值
  allretC <- allretC[,order(names(allretC))]
  return(allretC)
}

dodiff2 <- function() {
  ##抽的是summary
  metric0 <- scalefitresult(out0)
  scalar1 <- scalefitresult(out1)
  ##进一步抽参数
  metric0 <- data.matrix(metric0[6:28,])
  scalar1 <- data.matrix(scalar1[6:28,])
  ##做相减
  diff1 <- scalar1 - metric0
  ##save
  write.csv(metric0,file=paste0(outMCFA,"/metric0.csv"))
  write.csv(scalar1,file=paste0(outMCFA,"/scalar1.csv"))
  write.csv(diff1,file=paste0(outMCFA,"/diff1.csv"))

  #生成空矩阵存储delt卡方检验p值和是否显著的结果
  chidiffpvalue<-matrix(NA,1,ncol(diff1))
  chidiffptest<-matrix(NA,1,ncol(diff1))

  #进行delt卡方检验，存储P值到chidiffpvalue。
  for(i in 1:ncol(diff1)){
    tmpchidifft<-pchisq(diff1[c("ChiSqM_Value"),i], df=diff1[c("ChiSqM_DF"),i], lower.tail=F)
    chidiffpvalue[1,i]<-tmpchidifft
    if(chidiffpvalue[1,i]<0.05) {
      chidiffptest[1,i]<-0
    } #0是不等价，限定不成立
    else {
      #1是等价，限定成立
      chidiffptest[1,i]<-1
    }   
  }
  
  #拼接delta卡方检验结果
  cc1 <- diff1[c("ChiSqM_Value","ChiSqM_DF","CFI","TLI","AIC","BIC","RMSEA_Estimate","SRMR"),]
  scaletest<-rbind(cc1,chidiffpvalue,chidiffptest)
  #重命名
  rownames(scaletest)<-c("ChiSqM_Value","ChiSqM_DF","CFI","TLI","AIC","BIC","RMSEA_Estimate","SRMR","chidiffpvalue","chidiffptest")
  percentinvariance<-sum(scaletest[c("chidiffptest"),]==1)/ncol(diff1)*100
  percentvariance<-sum(scaletest[c("chidiffptest"),]==0)/ncol(diff1)*100
  #将delta卡方检验等结果存入csv文件
  write.csv(scaletest,file=paste0(outMCFA,"/scaleleveltest.csv"))
  ##单独保存下来
  ##print (percentinvariance)
  ##print (percentvariance)
  write.csv(percentinvariance,file=paste0(outMCFA,"/scalelevel_percent.csv"))
}

dodiff3 <- function() {
  metric0 <- scalefitresult(out0)
  item2 <- scalefitresult(out2)  
  
  write.csv(item2,file=paste0(outMCFA,"/itemtmp.csv"))
  ret1 <- data.frame(matrix(NA,23,0))

  for(i in 1:length(names(item2))){
    curname <- names(item2)[i]
    cur1 <- unlist(strsplit(curname, split="_"))
    cmpname <- cur1[1]
    base1 <- data.matrix(metric0[cmpname])
    exp1 <- data.matrix(item2[curname])
    base1 <- data.matrix(base1[6:28,])
    exp1 <- data.matrix(exp1[6:28,])
    diff1 <- exp1 - base1
    ret1[curname] <- diff1 
  }
  rownames(ret1)<-c("NGroups", "NDependentVars", "NIndependentVars", "NContinuousLatentVars", "Parameters", "ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue", "ChiSqBaseline_Value", "ChiSqBaseline_DF", "ChiSqBaseline_PValue", "LL", "UnrestrictedLL", "CFI", "TLI", "AIC", "BIC", "aBIC", "RMSEA_Estimate", "RMSEA_90CI_LB", "RMSEA_90CI_UB", "RMSEA_pLT05", "SRMR")
  write.csv(ret1,file=paste0(outMCFA,"/itemtmp2.csv"))

  #生成空矩阵存储delt卡方检验p值和是否显著的结果
  chidiffpvalue<-matrix(NA,1,ncol(ret1))
  chidiffptest<-matrix(NA,1,ncol(ret1))

  #进行delt卡方检验，存储P值到chidiffpvalue。
  for(i in 1:ncol(ret1)){
    tmpchidifft<-pchisq(ret1[c("ChiSqM_Value"),i], df=ret1[c("ChiSqM_DF"),i], lower.tail=F)
    chidiffpvalue[1,i]<-tmpchidifft
    if(chidiffpvalue[1,i]<0.05) {
      chidiffptest[1,i]<-0
    } #0是不等价，限定不成立
    else {
      #1是等价，限定成立
      chidiffptest[1,i]<-1
    }   
  }
  
  #拼接delta卡方检验结果
  cc1 <- ret1[c("ChiSqM_Value","ChiSqM_DF","CFI","TLI","AIC","BIC","RMSEA_Estimate","SRMR"),]
  write.csv(chidiffpvalue,file=paste0(outMCFA,"/itemleveltest_pvalue.csv"))
  write.csv(chidiffptest,file=paste0(outMCFA,"/itemleveltest_ptest.csv"))
  
  cc1 <- data.matrix(cc1)
  scaletest<-rbind(cc1,chidiffpvalue,chidiffptest)
  #重命名
  rownames(scaletest)<-c("ChiSqM_Value","ChiSqM_DF","CFI","TLI","AIC","BIC","RMSEA_Estimate","SRMR","chidiffpvalue","chidiffptest")
  #将delta卡方检验等结果存入csv文件
  write.csv(scaletest,file=paste0(outMCFA,"/itemleveltest_item.csv"))

  metri_scalar <- read.csv(paste0(outMCFA,"/scaleleveltest.csv"),header = TRUE)
  metri_scalar <- data.frame(metri_scalar)
  scaletest<-data.frame(scaletest)
  cnt1lst <- rep(0,Questionnum)
  cnt1sum <- rep(0,Questionnum)
  #统计1的个数
  for(i in 1:length(names(scaletest))){
    curname <- names(scaletest)[i]
    cur1 <- unlist(strsplit(curname, split="_"))
    sourcename <- cur1[1]
    qn <- cur1[2]
    qn <- as.numeric(qn)
    cnt1sum[qn] <- 1 + cnt1sum[qn]
    sourceptest <- metri_scalar[c(10),c(sourcename)]
    sourceptest <- as.numeric(sourceptest)
    print (curname)
    print (scaletest[c("chidiffptest"),c(curname)])
    if((scaletest[c("chidiffptest"),c(curname)] < 1) & (sourceptest < 1)){
      cnt1lst[qn] = 1 + cnt1lst[qn]
    }
  }
  #################################################
  ##整体检验是显著的，并且小题上的检验也是显著的 ##
  ##trial所占的比例                              ##
  #################################################
  cnt1ratio <- rep(0.0,Questionnum)
  for(q in 1:Questionnum){
    cnt1ratio[q] = cnt1lst[q]*1.0/cnt1sum[q]
  }
  write.csv(cnt1ratio,file=paste0(outMCFA,"/itemleveltest_ratio.csv"))
}

dodiff2()
dodiff3()

stopCluster(clusterobj)
