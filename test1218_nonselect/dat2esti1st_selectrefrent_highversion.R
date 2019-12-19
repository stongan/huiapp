
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
selerefpath <- paste0(curpath,SeleRefpath_1st)
if (!file.exists(selerefpath)){ dir.create(selerefpath, recursive=TRUE, showWarnings=FALSE) }
selerefg1 <- paste0(curpath,SeleRefMinG1_1st)
selerefg2 <- paste0(curpath,SeleRefMinG2_1st)
selerefg12 <- paste0(curpath,SeleRefMinG12_1st)
if (!file.exists(selerefg1)){ dir.create(selerefg1, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(selerefg2)){ dir.create(selerefg2, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(selerefg12)){ dir.create(selerefg12, recursive=TRUE, showWarnings=FALSE) }

##读入数据list进行遍历文件
tfname <- read.table(paste0(datapath,INPUTDATA_LIST))
tmatrix <- as.matrix(tfname)

##这是一个BIG Project，函数命名尽可能保证唯一性，Thx
estForSelect1st <- function(lst1, fname, fixidx, factoridx, resfile, model_var, model_model) {
  library(MplusAutomation)
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(fname,"_f",factoridx,"_fix",fixidx,".inp")
  
  ##Mplus 运行的参数配置
  ##COMMENT BY LiHui
  pathmodel <- mplusObject(
    TITLE = "this model is used to select intercept referent;",
    VARIABLE = model_var,
    MODEL = model_model,
    rdata = lst1)
  
  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
  return (fit)
}

##当前文件的main流程，包含主逻辑
##comment: 遍历每个文件
cccursize <- (loadingnum+interactionnum+factormeannum+interceptnum+varinum+residualnum)
tlihui <- paste(loadingnum,interactionnum,factormeannum,interceptnum,varinum,residualnum)
print (tlihui)
alldifftmp <- data.frame(matrix(NA,cccursize,0))
allratiotmp <- data.frame(matrix(NA,cccursize,0))
alllst1tmp <- data.frame(matrix(NA,cccursize,0))
alllst2tmp <- data.frame(matrix(NA,cccursize,0))
print("cccursize")
print(cccursize)

allfactor_minlst <- data.frame(matrix(NA,Factornum,0))
funclst <- c("NREPS","loadingnum","factormeannum",
               "interactionnum","interceptnum","varinum","residualnum",
               "Questionnum","Factornum","HasInterception","HasFactormean",
               "NGroup1st","Group1_samplesize","Group2_samplesize","getFactorCorr",
               "getdiff1st","dodiff","dodiffbyparam1st", "selectreferent_perfactor_helper",
               "selectreferent_perfactor", "selectreferent_perfactor_multi_helper",
               "getModelHeader","selectreferent_perfactor_multifactor",
               "selectreferent_perfactor_merge_0","sepAndMergeEst1st",
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
  factor_minlst <- rep(0, Factornum)
  for(factoridx in 1:Factornum){
    factor_minlst[factoridx] <- 1
  }
  allfactor_minlst[f2] <- factor_minlst
  print (f2)
  print ("factor_minlst")
  print (factor_minlst)
  curselectret_sep <- NULL
  if(Factornum==1){
    curselectret_sep <- selectreferent_perfactor(Questionnum, 1, 1, factor_minlst[1])
  } else {
    curselectret_sep <- selectreferent_perfactor_multifactor(Questionnum, Factornum, factor_minlst)
  }
  model_var_sep <- unlist(curselectret_sep[1])
  model_model_sep <- unlist(curselectret_sep[2])
  curfit_sep <- sepAndMergeEst1st(cur2, f2, factor_minlst, Factornum, selerefg1, model_var_sep, model_model_sep, 1)
  
  curselectret_merge <- selectreferent_perfactor_merge_0(Questionnum, Factornum, factor_minlst)
  model_var_merge <- unlist(curselectret_merge[1])
  model_model_merge <- unlist(curselectret_merge[2])
  curfit_merge <- sepAndMergeEst1st(cur2, f2, factor_minlst, Factornum, selerefg12, model_var_merge, model_model_merge, 1)
  #print ("model_var_merge")
  #print (model_var_merge)
  #print ("model_model_merge")
  #print (model_model_merge)
}

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
dodiff1st_high(selerefg1)
 
##
#
#write.csv(allfactor_minlst, file=paste0(selerefg1,"/intercept_referent_selected.csv"))
##print ("g1")
#print (alllst1tmp)
#print ("g2")
#print (alllst2tmp)
#print ("alldifftmp")
#print (alldifftmp)

stopCluster(clusterobj)

##Congratulate !!!
##You has finish first Part, Go ahead !
