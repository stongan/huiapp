
##该文件运行依赖dat2esti1st.R生成的文件数据「estimateG12/」

##依赖包及工作路径的初始化
library(foreach)
library(doSNOW)
library(MplusAutomation)
Mplus_command <- Mplus_CMD
scriptpath <- SCRIPT_PATH
setwd(scriptpath)

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
##参数顺序：nouse、文件名、nouse、结果路径、标签
G1diffG2 <- function(estlstnouse, f13, randnumnouse, resfile, idx){
  curwd <- paste0(scriptpath,resfile,"/simulatedata/",f13) 
  setwd(curwd)
  g1path <- paste0(curwd,"/G1_result")
  g2path <- paste0(curwd,"/G2_result")
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
#参数：数据、nouse、表情、文件名、输出文件路径
#I believe you have understand it
estimateCore <- function(lst1, fname, idx, f1, resfile) {
  library(MplusAutomation)
  setwd(resfile)
  retfile1 <- paste0(f1,"G",idx,".inp")
  retfile2 <- paste0(f1,"G",idx,"_param.dat")
  rettmp <- paste0("RESULTS = ",retfile2,";")  
  pathmodel <- mplusObject(
    TITLE = "MplusAutomation Example - Path Model;",
    DATA = "LISTWISE = ON;",
    VARIABLE = "NAMES = V1-V11;
    USEVARIABLES = V1-V10;",
    ANALYSIS = "TYPE = GENERAL;
    MODEL = NOMEANSTRUCTURE;
    INFORMATION = EXPECTED;",
    MODEL = "f1 by V1* V2-V10;
    f1@1;",
    rdata = lst1)
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
}

##1-2 根据callMplus生成的模拟数据再跑一次模型
callEstimate <- function(estlst, f12, randnum, resfile, idx) {
  ##设置模型生成的路径分G1和G2跑 
  estipath <- paste0(scriptpath,resfile,"/simulatedata/",f12)
  lstfile <- paste0(estipath,"/G12simu_",f12,"list.dat")
  t_outG1 <- paste0(estipath,"/G1_result")
  t_outG2 <- paste0(estipath,"/G2_result")
  if (!file.exists(t_outG1)){ dir.create(t_outG1, recursive=TRUE, showWarnings=FALSE) }
  if (!file.exists(t_outG2)){ dir.create(t_outG2, recursive=TRUE, showWarnings=FALSE) }
  ##读取数据的list 
  print (f12)
  print ("f12")
  tfname <- read.table(lstfile)
  tmatrix <- as.matrix(tfname)
  
  ##遍历每一个文件
  for(r in 1:nrow(tmatrix)){
    setwd(estipath)
    filename <- tmatrix[r]
    fileone <- paste0(estipath,"/",filename)  
    cur1 <- read.table(fileone)
    cur2 <- data.frame(cur1)
    
    cur2man <- subset(cur2, V11==1)
    cur2fem <- subset(cur2, V11==2)
    
    filetmpgroup <- unlist(strsplit(filename, split="\\."))
    f2 <- filetmpgroup[1]
    
    #fileone no using
    #参数：数据、nouse、表情、文件名、输出文件路径
    estimateCore(cur2man,fileone,1,f2,t_outG1)
    estimateCore(cur2fem,fileone,1,f2,t_outG2)
  }
}

##被mainfunc2rd 调用
##1-1 调用mplus生成模拟数据
##估计参数矩阵、文件名、随机数、生成模拟数的保存路径、标签其实就是G12
##注意修改重复次数
callMplus <- function(estlst, f1, randnum, resfile, idx) {
  library(MplusAutomation)
  ##新建模拟数据的保存路径并设置工作路径
  curwd_t <- paste0(scriptpath,resfile,"/simulatedata/",f1) 
  if (!file.exists(curwd_t)){ dir.create(curwd_t, recursive=TRUE, showWarnings=FALSE) }
  setwd(curwd_t)
  ##拼接文件名及中间参数文件名
  retfile1 <- paste0(f1,"simulate",idx,".inp")
  retfile2 <- paste0(f1,"simulate",idx,"_param.dat")
  ##以下为拼接参数矩阵到模型的参数
  rettmp <- paste0("RESULTS = ",retfile2,";")  
  t_monte <- "NAMES = y1-y10;
    NOBS = 150 150;
    NGROUPS=2;
    nreps="
  t_monte <- paste0(t_monte,NREPS)
  t_monte <- paste0(t_monte,";seed=")
  t_monte <- paste0(t_monte,randnum)
  t_monte2 <- ";REPSAVE = ALL;
    save = G12simu_"
  t_monte3 <- "*.dat;"
  t_monte <- paste0(t_monte,t_monte2,f1,t_monte3)
  ##循环凭借参数
  t_popu <- "f1 by "
  for(i in 1:length(estlst)){
    yvalue <- i
    if(i < 11){
      t_popu <- paste0(t_popu,"y",yvalue,"*",estlst[i],"\n") 
    } else if(i > 11){
      yvalue <- i-11
      t_popu <- paste0(t_popu,"y",yvalue,"*",estlst[i],"\n") 
    } else {
      t_popu <- paste0(t_popu, ";")
    }
  } 
  t_popu <- paste0(t_popu, "; f1@1; [f1@0];")
  ##执行模拟并生产数据
  pathmodel <- mplusObject(
    TITLE = "data generation based on estimates of G12;",
    MONTECARLO = t_monte,
    MODELPOPULATION = t_popu,
    OUTPUT= "TECH9;")
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
  funclst <- c("callMplus","callEstimate","estimateCore","G1diffG2","G1dG2Core","NREPS")
  ##遍历路径下的每一个.out文件
  for(i in 1:length(names(paraG12))){
  #foreach(i=1:length(names(paraG12)), .verbose=T, .export=funclst, .combine="c") %dopar% {
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
    ##1-3 计算每一个G1diffG2
    G1diffG2(estlst, cname[i], randomlst[i], curpath, idx)
  }
}

##主调用逻辑main逻辑
##参数顺序：G12路径，G12标签
mainfunc2rd(G12path_1st, G12flag)

