
##该文件运行依赖dat2esti1st.R生成的文件数据「estimateG12/」

##依赖包及工作路径的初始化
library(foreach)
library(doSNOW)
library(MplusAutomation)

source("util_intercept_xing1.R")

Mplus_command <- Mplus_CMD
scriptpath <- SCRIPT_PATH
setwd(scriptpath)
njobs <- NJOBS
setDefaultClusterOptions(master="localhost")
clusterobj <- makeSOCKcluster(njobs)
registerDoSNOW(clusterobj)

##被G1diffG2调用
##1-3-1 计算每一个G1diffG2
##参数顺序：文件路径
G1dG2Core <- function(extrapath){
  fileout <- extrapath 
  paraG12 <- readModels(fileout,recursive =TRUE, what="all")
  
  estlen <- length(paraG12[[1]]$parameters$unstandardized$est)
  cname <- rep("NA",length(names(paraG12)))
  allret <- data.frame(matrix(NA,estlen,0))
  randomlst <- c(sample(1000:100000,length(names(paraG12)), replace = F))
  for(i in 1:length(names(paraG12))){
    curname <- names(paraG12)[i]
    cur1 <- unlist(strsplit(curname, split="\\."))
    cname[i] <- cur1[length(cur1)-1]
    estlst <- paraG12[[curname]]$parameters$unstandardized$est
    allret[cname[i]] <- as.vector(estlst)
  }
  return (allret)
}

##1-3 计算每一个G1diffG2
##参数顺序：文件名、结果路径、标签
G1diffG2 <- function(f13, resfile, idx){
  curwd <- paste0(scriptpath,resfile,"/simulatedata/",f13) 
  setwd(curwd)
  g1path <- paste0(curwd,"/G1_result")
  g1dataframe <- G1dG2Core(g1path)
  g2dataframe <- G1dG2Core(g2path)
  retdiff <- g1dataframe - g2dataframe
  retratio <- g1dataframe / g2dataframe

  g1datafile <- paste0(curwd,"/g1.csv")
  g2datafile <- paste0(curwd,"/g2.csv")
  write.csv(g1dataframe, file=g1datafile)
  write.csv(g2dataframe, file=g2datafile)
  retfile <- paste0(curwd,"/diff.csv")
  write.csv(retdiff, file=retfile)
  retfile2 <- paste0(curwd,"/ratio.csv")
  write.csv(retratio, file=retfile2)
}

#1-2-1 invoke by callEstimate
estForSelect2rd <- function(lst1, fname, resfile, model_var, model_model) {
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

##1-2 根据callMplus生成的模拟数据再跑一次模型
callEstimate <- function(f12, resfile) {
  ##设置模型生成的路径分G1和G2跑 
  estipath <- paste0(scriptpath,resfile,"/simulatedata/",f12)
  lstfile <- paste0(estipath,"/",f12,"_list.dat")
  t_outG1 <- paste0(estipath,"/G1_result")
  if (!file.exists(t_outG1)){ dir.create(t_outG1, recursive=TRUE, showWarnings=FALSE) }
  ##读取数据的list
  
  print (f12)
    
  tfname <- read.table(lstfile)
  tmatrix <- as.matrix(tfname)
 
  cccursize <- (loadingnum+interactionnum+varinum+residualnum)
  alldifftmp <- data.frame(matrix(NA,cccursize,0))
  allratiotmp <- data.frame(matrix(NA,cccursize,0))
  alllst1tmp <- data.frame(matrix(NA,cccursize,0))
  alllst2tmp <- data.frame(matrix(NA,cccursize,0))
  ##遍历每一个文件
  for(r in 1:nrow(tmatrix)){
    setwd(estipath)
    filename <- tmatrix[r]
    fileone <- paste0(estipath,"/",filename)  
    cur1 <- read.table(fileone)
    cur2 <- data.frame(cur1)
    
    filetmpgroup <- unlist(strsplit(filename, split="\\."))
    f2 <- filetmpgroup[1]
    
    #参数：数据、文件名、输出文件路径
    model_list <- selectv1(Questionnum, Factornum)
    model_var <- unlist(model_list[1])
    model_model <- unlist(model_list[2])

    curfit_sep <- estForSelect2rd(cur2, f2, t_outG1, model_var, model_model) 
        
    curallret <- getdiff1st(curfit_sep)
    curdiff <- unlist(curallret[1])
    curratio <- unlist(curallret[2])
    curlst1 <- unlist(curallret[3])
    curlst2 <- unlist(curallret[4])

    alldifftmp[f2] <- curdiff
    allratiotmp[f2] <- curratio
    alllst1tmp[f2] <- curlst1
    alllst2tmp[f2] <- curlst2
    unlink(fileone)
  }
  write.csv(alldifftmp, file=paste0(estipath,"/diff.csv"))
  write.csv(allratiotmp, file=paste0(estipath,"/ratio.csv"))
  write.csv(alllst1tmp, file=paste0(estipath,"/g1.csv"))
  write.csv(alllst2tmp, file=paste0(estipath,"/g2.csv"))
}

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

##被mainfunc2rd 调用
##1-1 调用mplus生成模拟数据
##估计参数矩阵、文件名、随机数、生成模拟数的保存路径、标签其实就是G12
##注意修改重复次数
callMplus <- function(estlst, f1, randnum, resfile, idx) {
  library(MplusAutomation)
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
    
  ##新建模拟数据的保存路径并设置工作路径
  curwd_t <- paste0(scriptpath,resfile,"/simulatedata/",f1) 
  if (!file.exists(curwd_t)){ dir.create(curwd_t, recursive=TRUE, showWarnings=FALSE) }
  setwd(curwd_t)
  ##拼接文件名及中间参数文件名
  retfile1 <- paste0(f1,"simulate",".inp")
  t_monte <- paste0("NAMES = y1-y",Questionnum,";NOBS = ")
  t_monte <- paste0(t_monte,Group1_samplesize," ",Group2_samplesize,";")
  t_monte <- paste0(t_monte,"NGROUPS=",NGroup1st,";")
  t_monte <- paste0(t_monte,"nreps=")
  t_monte <- paste0(t_monte,NREPS)
  t_monte <- paste0(t_monte,";seed=")
  t_monte <- paste0(t_monte,randnum)
  t_monte2 <- ";REPSAVE = ALL;
    save = "
  t_monte3 <- "*.dat;"
  t_monte <- paste0(t_monte,t_monte2,f1,"_",t_monte3)
  ##循环凭借参数
  ####################################################################
  ##"f1 by y1*1 y2*2 y3*3 y4*4 y5*5 ... y19*19 y20*20 ;              #
  ## f2 by y21*21 y22*22 y23*23 y24*24 y25*25 ... y39*39 y40*40 ;    #
  ## ...                                                             #
  ## f5 by y81*81 y82*82 y83*83 y84*84 y85*85 ... y99*99 y100*100 ;" #
  ####################################################################
  ItemnumPerFactor <- as.integer(Questionnum/Factornum)
  t_popu <- ""
  for(i_fact in 1:Factornum){
    t_popu <- paste0(t_popu,"f",i_fact," by ")
    for(i in 1:ItemnumPerFactor){
      tkey <- ((i_fact-1)*ItemnumPerFactor + i)
      curload <- loadlst[tkey]
      t_popu <- paste0(t_popu,"y",tkey,"*",curload,"\n")
    }
    t_popu <- paste0(t_popu,";")
  }

  for(i in 1:length(residlst)){
    t_popu <- paste0(t_popu,"y",i,"*",residlst[i],"\n")
  }
  t_popu <- paste0(t_popu, ";")
  if(HasInterception > 0){
    for(i in 1:length(interceptlst)){
      t_popu <- paste0(t_popu,"[y",i,"*",interceptlst[i],"];\n")
    }
  }

  for(i in 1:length(varilst)){
    t_popu <- paste0(t_popu," f",i,"*",varilst[i],";\n")
  }

  if(factormeannum > 0){
    for(i in 1:length(factormeanlst)){
      t_popu <- paste0(t_popu," [f",i,"*",factormeanlst[i],"];\n")
    }
  }

  k <- 1
  if(Factornum>1){
    for (i in 2:Factornum) {
      for (j in 1:(i-1)) {
        t_popu<-paste0(t_popu,"f",i," with ","f",j,"*",interactionlst[k],";\n")
        k <- k+1
      }
    }
  }
  print (t_popu)
  
  ##执行模拟并生成数据
  pathmodel <- mplusObject(
    TITLE = "data generation based on estimates of G12;",
    MONTECARLO = t_monte,
    MODELPOPULATION = t_popu,
    OUTPUT= "TECH1;")
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
}

##1 主逻辑函数
##参数顺序：G12路径，G12标签
mainfunc2rd <- function(curpath, idx) {
  ##拼接得到完整路径,example: ./estimateG12/
  fileout <- paste0(scriptpath,curpath)
  ##读取fileout路径下的所有.out文件，然后抽参数
  paraG12 <- readModels(fileout, what="all")
  ##并发操作造成的路径问题
  scriptpath <- scriptpath
  ##拿一个样本来取每个文件的样本估计值的个数
  estlen <- length(paraG12[[1]]$parameters$unstandardized$est)
  ##初始化一个列名的list
  cname <- rep("NA",length(names(paraG12)))
  ##初始化一个结果矩阵
  allret <- data.frame(matrix(NA,estlen,0))
  ##初始化一个随机数的list
  randomlst <- c(sample(1000:100000, length(names(paraG12)), replace = F))
  ##并发操作需要的函数,需要显示指定
  funclst <- c("callMplus","callEstimate","G1diffG2",
               "G1dG2Core","NREPS","loadingnum","factormeannum",
               "interactionnum","interceptnum","varinum","residualnum",
               "Questionnum","Factornum","HasInterception","HasFactormean",
               "NGroup1st","Group1_samplesize","Group2_samplesize","getFactorCorr",
               "getdiff1st","estForSelect2rd", 
               "getModelHeader", "getModelHeaderA", "getModelHeaderB", 
	       "getModelHeader_merge", "selectv1")
  ##遍历路径下的每一个.out文件
  ##for(i in 1:length(names(paraG12))){
  foreach(i=1:length(names(paraG12)), .verbose=T, .export=funclst, .combine="c") %dopar% {
    ##之后的函数调用设计改变路径，每个循环初始化路径
    setwd(scriptpath)
    ##取当前文件名字
    curname <- names(paraG12)[i]
    ##切割文件名字，只要后缀名之前的一段
    cur1 <- unlist(strsplit(curname, split="\\."))
    cname[i] <- cur1[length(cur1)-1]
    ##抽每个out文件的参数
    estlst <- paraG12[[curname]]$parameters$unstandardized$est
    ##按文件名保存到矩阵
    allret[cname[i]] <- as.vector(estlst)
    ##1-1 调用mplus生成模拟数据
    callMplus(estlst, cname[i], randomlst[i], curpath, idx)
    ##1-2 根据上面的模拟数据再跑一次模型
    callEstimate(cname[i],curpath)
  }
  
  ##为了得到中间参数，多线程中的写失效
  for(i in 1:length(names(paraG12))){
    curname <- names(paraG12)[i]
    cur1 <- unlist(strsplit(curname, split="\\."))
    cname[i] <- cur1[length(cur1)-1]
    estlst <- paraG12[[curname]]$parameters$unstandardized$est
    allret[cname[i]] <- as.vector(estlst)
  }
  ##保存中间数据-抽取出来估计参数矩阵
  retfile <- paste0(fileout,"/param",idx,".csv")
  write.csv(allret, file=retfile)
}

##主调用逻辑main逻辑
##参数顺序：G12路径，G12标签
mainfunc2rd(SeleRefMinG12_1st, G12flag)
stopCluster(clusterobj)

