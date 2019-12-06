
##依赖包及工作路径的初始化
library(MplusAutomation)
Mplus_command <- Mplus_CMD
datapath <- INPUTDATA
curpath <- SCRIPT_PATH
setwd(curpath)

##建立输出文件路径
outG1 <- paste0(curpath,G1path_1st)
outG2 <- paste0(curpath,G2path_1st)
outG12 <- paste0(curpath,G12path_1st)
if (!file.exists(outG1)){ dir.create(outG1, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(outG2)){ dir.create(outG2, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(outG12)){ dir.create(outG12, recursive=TRUE, showWarnings=FALSE) }

##读入数据list进行遍历文件
tfname <- read.table(paste0(datapath,INPUTDATA_LIST))
tmatrix <- as.matrix(tfname)

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
  #print("loadlst:")
  #print(loadlst)
  #print("interactionlst")
  #print(interactionlst) 
  #print("factormeanlst")
  #print(factormeanlst)
  #print("interceptlst")
  #print(interceptlst)
  #print("varilst")
  #print(varilst)
  #print("residlst")
  #print(residlst)
  #print("loadlst2:")
  #print(loadlst2)
  #print("interactionlst2")
  #print(interactionlst2) 
  #print("factormeanlst2")
  #print(factormeanlst2)
  #print("interceptlst2")
  #print(interceptlst2)
  #print("varilst2")
  #print(varilst2)
  #print("residlst2")
  #print(residlst2)
}
  
#这是一个选择函数
seperateEst1st <- function(lst1, f1, resfile) {
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(f1,".inp")

  ##Mplus 运行的参数配置
  ##COMMENT BY LiHui
  pathmodel <- mplusObject(
   TITLE = "esitimate g1 and g2 seperately, fixing factor loadings equal ;",
   VARIABLE = "NAMES = V1-V101;
               USEVARIABLES = V1-V100;
               grouping = V101(1=g1 2=g2);",
   MODEL = "f1 by V1@1 V2-V20;
            f2 by V21@1 V22-V40;
            f3 by V41@1 V42-V60;
            f4 by V61@1 V62-V80;
            f5 by V81@1 V82-V100;
            [f1-f5@0];
            f1-f5*;
            V1-V100*;
            [V1-V100*];
   MODEL g2:
            [f1-f5@0];
            f1-f5*;
            V1-V100*;
            [V1-V100*];
            f2 with f1;
            f3 with f1;
            f3 with f2;
            f4 with f1;
            f4 with f2;
            f4 with f3;
            f5 with f1;
            f5 with f2;
            f5 with f3;
            f5 with f4;",
   OUTPUT = "stdyx tech1;",
   rdata = lst1)

  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
  return(fit)
}

##这是一个BIG Project，函数命名尽可能保证唯一性，Thx
geneEst1st <- function(lst1, f1, resfile) {
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(f1,".inp")

  ##Mplus 运行的参数配置
  ##COMMENT BY LiHui
  pathmodel <- mplusObject(
   TITLE = "estimate group1 and group2 together;",
   VARIABLE = "NAMES = V1-V101;
               USEVARIABLES = V1-V100;",
   MODEL = "f1 by V1@1 V2-V20;
            f2 by V21@1 V22-V40;
            f3 by V41@1 V42-V60;
            f4 by V61@1 V62-V80;
            f5 by V81@1 V82-V100;
            [f1-f5@0];
            f1-f5*;
            V1-V100*;
            [V1-V100*];
            f2 with f1;
            f3 with f1;
            f3 with f2;
            f4 with f1;
            f4 with f2;
            f4 with f3;
            f5 with f1;
            f5 with f2;
            f5 with f3;
            f5 with f4;",
   OUTPUT = "stdyx tech1;",
   rdata = lst1)

  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
}


##当前文件的main流程，包含主逻辑
##comment: 遍历每个文件
setwd(curpath)
filename <- tmatrix[1]
fileone <- paste0(datapath,"/",filename)  
cur1 <- read.table(fileone)
cur2 <- data.frame(cur1)
filetmpgroup <- unlist(strsplit(filename, split="\\."))
f2 <- filetmpgroup[1]
firstfit <- seperateEst1st(cur2,f2,outG1)
curlen <- length(firstfit$results$parameters$unstandardized$est)
cccursize <- curlen / NGroup1st
alldifftmp <- data.frame(matrix(NA,cccursize,0))
allratiotmp <- data.frame(matrix(NA,cccursize,0))
alllst1tmp <- data.frame(matrix(NA,cccursize,0))
alllst2tmp <- data.frame(matrix(NA,cccursize,0))

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
  
  ##抽取文件名不要后缀；example: lihuifile.dat -> lihuifile
  filetmpgroup <- unlist(strsplit(filename, split="\\."))
  f2 <- filetmpgroup[1]
 
  ##执行模型的接口，参数顺序：数据、文件路径、文件标签、不带后缀文件名、输出路径
  geneEst1st(cur2,f2,outG12)
  curfit <- seperateEst1st(cur2,f2,outG1)
  if(r == 1){
    curfit <- firstfit
  }
  curallret <- getdiff1st(curfit)
  curdiff <- curallret[1]
  curratio <- curallret[2]
  curlst1 <- curallret[3]
  curlst2 <- curallret[4]
  alldifftmp[f2] <- curdiff
  allratiotmp[f2] <- curratio
  alllst1tmp[f2] <- curlst1
  alllst2tmp[f2] <- curlst2
}

write.csv(alldifftmp, file=paste0("./g1diff.csv"))
write.csv(allratiotmp, file=paste0("./g1ratio.csv"))
write.csv(alllst1tmp, file=paste0("./paramG1.csv"))
write.csv(alllst2tmp, file=paste0("./paramG2.csv"))
print (alldifftmp)
##Congratulate !!!
##You has finish first Part, Go ahead !
