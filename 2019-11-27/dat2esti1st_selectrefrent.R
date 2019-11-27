Mplus_command <- "/opt/mplus/8.3/mplus"
##依赖包及工作路径的初始化
library(MplusAutomation)
Mplus_command <- Mplus_CMD
datapath <- INPUTDATA
curpath <- SCRIPT_PATH
setwd(curpath)

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

getLameda <- function(fit1) {
  t <- fit1$results$parameters$unstandardized$est
  curlen <- length(t)
  cursize <- curlen / NGroup1st
  lst1 <- t[1:Questionnum]
  lst2 <- t[(cursize+1):(cursize+Questionnum)]
  diff1 <- lst1 - lst2
  #print (class(t))
  #print (length(t))
  #print ("t:")
  #print (t)
  #print ("lst1:")
  #print (lst1)
  #print ("cursize:")
  #print (cursize)
  #print ("lst2:")
  #print (lst2)
  #print ("cursize:")
  #print (cursize)
  #print (diff1)
  for (i in 1:Questionnum){
    if(diff1[i] < 0){
      diff1[i] <- diff1[i]*-1
    }
  }
  return (diff1)
}

##这是一个BIG Project，函数命名尽可能保证唯一性，Thx
geneEst1st <- function(lst1, fname, f1, resfile, curequal, dfidx) {
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(f1,"_r",dfidx,".inp")

  ##Mplus 运行的参数配置
  ##COMMENT BY LiHui
  pathmodel <- mplusObject(
   TITLE = "select referent item;",
   VARIABLE = "NAMES = V1-V11;
               USEVARIABLES = V1-V10;
               grouping = V11(2);",
   ANALYSIS = "model = configural;",
   MODEL = curequal,
   rdata = lst1)

  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
  return (getLameda(fit))
}
##这是一个BIG Project，函数命名尽可能保证唯一性，Thx
generefEst1st <- function(lst1, fname, idx, f1, resfile, curequal1, minIdx) {
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(f1,"G",idx,"__",minIdx,".inp")
  
  ##Mplus 运行的参数配置
  ##COMMENT BY LiHui
  pathmodel <- mplusObject(
    TITLE = "MplusAutomation dat2esti1st Model;",
    DATA = "LISTWISE = ON;",
    VARIABLE = "NAMES = V1-V11;
    USEVARIABLES = V1-V10;",
    ANALYSIS = "TYPE = GENERAL;
    MODEL = NOMEANSTRUCTURE;
    INFORMATION = EXPECTED;",
    MODEL = curequal1,
    rdata = lst1)
  
  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
}

getCurEqual <- function(dfidx) {
  curret <- ""
  if(dfidx == 1){
    curret <-"f1 by V1@1"
  } else {
    curret <-"f1 by V1*"
  } 
  for(j in 2:Questionnum) {
    curret <- paste0(curret," V",j)
    if(dfidx != 1 && dfidx == j){
        curret <- paste0(curret,"@1")
    }
  }
  curret <- paste0(curret,";")
  return (curret)
} 

##当前文件的main流程，包含主逻辑
##comment: 遍历每个文件
tdebug <- ""
for(r in 1:nrow(tmatrix)){
  ##为了确保每个model输出到对应的文件夹，每次循环后重置路径
  setwd(curpath)
  ##得到每一个文件名
  filename <- tmatrix[r]
  ##拼接文件路径得到文件的具体路径
  fileone <- paste0(datapath,filename)
  ##读文件并转成data.frame,可能是冗余的，读进来就是data.frame.
  cur1 <- read.table(fileone)
  cur2 <- data.frame(cur1)
  
  ##条件筛选，主要分男女，优化点：文件配置形式及UI接口，这个优化值1w
  cur2man <- subset(cur2, V11==1)
  cur2fem <- subset(cur2, V11==2)
  
  ##抽取文件名不要后缀；example: lihuifile.dat -> lihuifile
  filetmpgroup <- unlist(strsplit(filename, split="\\."))
  f2 <- filetmpgroup[1]
  sumdeta <- rep(0.0,Questionnum)
  for(dfidx in 1:Questionnum) {
    curequal <- getCurEqual(dfidx)
    ##执行模型的接口，参数顺序：数据、文件路径、文件标签、不带后缀文件名、输出路径
    curdeta <- geneEst1st(cur2,fileone,f2,selerefpath,curequal,dfidx)
    sumdeta <- sumdeta + curdeta
  }
  sumdeta <- sumdeta / (Questionnum-1)
  #print (min(sumdeta))
  minIdx = 1
  for(k in 1:Questionnum){
    tofind = as.character(min(sumdeta))
    if(as.character(sumdeta[k]) == tofind){
      minIdx = k
    }
  }
  print (minIdx)
  curequal <- getCurEqual(minIdx)
  generefEst1st(cur2, fileone, 12, f2, selerefg12, curequal, minIdx)
  generefEst1st(cur2man, fileone, 1, f2, selerefg1, curequal, minIdx)
  generefEst1st(cur2fem, fileone, 2, f2, selerefg2, curequal, minIdx)
  tdebug <- paste0(tdebug,minIdx,"|")
}
print (tdebug)
##Congratulate !!!
##You has finish first Part, Go ahead !
