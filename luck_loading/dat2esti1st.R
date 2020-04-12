
##依赖包及工作路径的初始化
library(MplusAutomation)
library(foreach)
library(doSNOW)
Mplus_command <- Mplus_CMD
datapath <- INPUTDATA
curpath <- SCRIPT_PATH
setwd(curpath)
njobs <- NJOBS
setDefaultClusterOptions(master="localhost")
clusterobj <- makeSOCKcluster(njobs)
registerDoSNOW(clusterobj)

source("util_intercept_xing1.R")

##建立输出文件路径
selerefpath <- paste0(curpath,SeleRefMinG1_1st)
if (!file.exists(selerefpath)){ dir.create(selerefpath, recursive=TRUE, showWarnings=FALSE) }

##读入数据list进行遍历文件
tfname <- read.table(paste0(datapath,INPUTDATA_LIST))
tmatrix <- as.matrix(tfname)

##这是一个BIG Project，函数命名尽可能保证唯一性，Thx
estForSelect1st <- function(lst1, fname, resfile, model_var, model_model) {
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

##当前文件的main流程，包含主逻辑
allfactor_minlst <- data.frame(matrix(NA,Factornum,0))
funclst <- c("NREPS","loadingnum","factormeannum",
               "interactionnum","interceptnum","varinum","residualnum",
               "Questionnum","Factornum","HasInterception","HasFactormean",
               "NGroup1st","Group1_samplesize","Group2_samplesize",
               "getModelHeader","getModelHeaderA", "getModelHeaderB", "selectv1",
	       "estForSelect1st")

foreach(r=1:nrow(tmatrix), .verbose=T, .export=funclst, .combine="c") %dopar% {
##for(r in 1:nrow(tmatrix)){

  ##为了确保每个model输出到对应的文件夹，每次循环后重置路径
  setwd(curpath)
  ##得到每一个文件名
  filename <- tmatrix[r]
  ##拼接文件路径得到文件的具体路径
  fileone <- paste0(datapath,filename)
  ##读文件并转成data.frame,可能是冗余的，读进来就是data.frame.
  cur1 <- read.table(fileone)
  cur2 <- data.frame(cur1)
  
  ##抽取文件名不要后缀；example: lihuifile.dat -> lihuifile
  filetmpgroup <- unlist(strsplit(filename, split="\\."))
  f2 <- filetmpgroup[1]

  model_list <- selectv1(Questionnum, Factornum)
  model_var <- unlist(model_list[1])
  model_model <- unlist(model_list[2])
  
  estForSelect1st(cur2, f2, selerefpath, model_var, model_model)
}

cccursize <- (loadingnum+interactionnum+varinum+residualnum)
#下面这两句没用到吧
tlihui <- paste(loadingnum,interactionnum,factormeannum,interceptnum,varinum,residualnum)
print (tlihui)
#
alldifftmp <- data.frame(matrix(NA,cccursize,0))
allratiotmp <- data.frame(matrix(NA,cccursize,0))
alllst1tmp <- data.frame(matrix(NA,cccursize,0))
alllst2tmp <- data.frame(matrix(NA,cccursize,0))

#对selerefg1做
dodiff1st_high <- function(pinpath) {
  fileout <- paste0(pinpath)
  ##读取fileout路径下的所有.out文件，然后抽参数
  paraG1 <- readModels(fileout, what="all")

  for(i in 1:length(names(paraG1))){
    curname <- names(paraG1)[i]
    ##切割文件名字，只要后缀名之前的一段
    cur1 <- unlist(strsplit(curname, split="\\."))
    cur2 <- unlist(strsplit(cur1[length(cur1)-1], split="_"))
    f2 <- cur2[1]

    curallret <- getdiff1st_v2(paraG1[[curname]])
    curdiff <- unlist(curallret[1])
    curratio <- unlist(curallret[2])
    curlst1 <- unlist(curallret[3])
    curlst2 <- unlist(curallret[4])
    alldifftmp[f2] <- curdiff
    allratiotmp[f2] <- curratio
    alllst1tmp[f2] <- curlst1
    alllst2tmp[f2] <- curlst2
  }
  write.csv(alldifftmp, file=paste0(pinpath,"/g1diff.csv"))
  write.csv(allratiotmp, file=paste0(pinpath,"/g1ratio.csv"))
  write.csv(alllst1tmp, file=paste0(pinpath,"/paramG1.csv"))
  write.csv(alllst2tmp, file=paste0(pinpath,"/paramG2.csv"))
}
dodiff1st_high(selerefpath)

stopCluster(clusterobj)

##Congratulate !!!
##You has finish first Part, Go ahead !
