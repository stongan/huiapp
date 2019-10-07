
##该文件运行依赖dat2esti1st.R生成的文件数据「estimateG1、estimateG2」

##依赖包及工作路径的初始化
library(MplusAutomation)
Mplus_command <- Mplus_CMD
scriptpath <- SCRIPT_PATH
setwd(scriptpath)

interval95H<-function(x,alpha){
  xbar<-mean(x)
  sdx<-sd(x)
  #n<-length(x)-1
  #interval_h <- xbar+qt(1-alpha/2,n-1)*sdx/sqrt(n)
  interval_h<-xbar+1.96*sdx
  return(interval_h)
}
interval95L<-function(x,alpha){
  xbar<-mean(x)
  sdx<-sd(x)
  #n<-length(x)-1
  #interval_l <- xbar-qt(1-alpha/2,n-1)*sdx/sqrt(n)
  interval_l<-xbar-1.96*sdx
  return(interval_l)
}

##数据抽取路径、标签
extraEst <- function(curpath, idx) {
  fileout <- paste0(getwd(),curpath) 
  paraG12 <- readModels(fileout,recursive =TRUE, what="all")
  
  estlen <- length(paraG12[[1]]$parameters$unstandardized$est)
  cname <- rep("NA",length(names(paraG12)))
  allret <- data.frame(matrix(NA,estlen,0))
  #randomlst <- c(sample(1000:100000, length(names(paraG12)), replace = F))
  for(i in 1:length(names(paraG12))){  
    setwd(scriptpath)
    curname <- names(paraG12)[i]
    cur1 <- unlist(strsplit(curname, split="\\."))
    cname[i] <- cur1[length(cur1)-1]
    estlst <- paraG12[[curname]]$parameters$unstandardized$est
    allret[cname[i]] <- as.vector(estlst)
  }

  retfile <- paste0(fileout,"/param",idx,".csv")
  write.csv(allret, file=retfile)
  return (allret)
}

##main逻辑开始
##genefirstG1 <- function(){
##做数据抽取并保存
G1df <- extraEst(G1path_1st, "G1")
G2df <- extraEst(G2path_1st, "G2")
G1diffG2 <- G1df - G2df
G1ratioG2 <- G1df / G2df
t_row <- nrow(G1ratioG2)
t_col <- length(G1ratioG2)
G1ratioG2med <- G1ratioG2
for(j in 1:t_col){
  lst1 <- G1ratioG2[1:Questionnum,j]
  med1 <- median(lst1)
  #print (lst1)
  #print (med1)
  G1ratioG2med[,j] <- G1ratioG2[,j] / med1
}
write.csv(G1diffG2, file=paste0(scriptpath,"/estimateG1/g1diff.csv"))
write.csv(G1ratioG2, file=paste0(scriptpath,"/estimateG1/g1ratio.csv"))
##}

helperH <- function(tdata){
  retlst <- c()
  for(i in 1:nrow(tdata)){
    huidata <- unlist(tdata[i:i,])
    t1 <- interval95H(huidata,0.05)
    #print (t1)
    retlst <- c(retlst, t1)
  }
  return (retlst)
}
helperL <- function(tdata){
  retlst <- c()
  for(i in 1:nrow(tdata)){
    #print (tdata[i:i,])
    huidata <- unlist(tdata[i:i,])
    t1 <- interval95L(huidata,0.05)
    #print (t1)
    retlst <- c(retlst, t1)
  }
  #print (retlst)
  return (retlst)
}
getG12HLinterval <- function(){
  simupath <- paste0(scriptpath,"/estimateG12/simulatedata/")
  alst <- list.files(simupath)
  
  #just getlen
  data1 <- read.csv(paste0(simupath,alst[1],"/diff.csv"),header = TRUE)
  data1 <- data1[,-1]
  alen <- nrow(data1)
  #alen2 <- length(data1)
  retdiffH <- data.frame(matrix(NA,alen,0))
  retdiffL <- data.frame(matrix(NA,alen,0))
  retratioH <- data.frame(matrix(NA,alen,0))
  retratioL <- data.frame(matrix(NA,alen,0))
  for(i in 1:length(alst)){
    fname <- alst[i]
    data1 <- read.csv(paste0(simupath,fname,"/diff.csv"),header = TRUE)
    data2 <- read.csv(paste0(simupath,fname,"/ratio.csv"),header = TRUE)
    data1 <- data1[,-1]
    data2 <- data2[,-1]
    inter1 <- helperH(data1)
    inter2 <- helperL(data1)
    inter3 <- helperH(data2)
    inter4 <- helperL(data2)
    #print(inter1)
    retdiffH[fname] <- as.vector(inter1)
    retdiffL[fname] <- as.vector(inter2)
    retratioH[fname] <- as.vector(inter3)
    retratioL[fname] <- as.vector(inter4)
  }
  #print (retdiffH)
  #print (retratioH)
  write.csv(retdiffH, file=paste0(scriptpath,"/estimateG1/g12diffH.csv"))
  write.csv(retdiffL, file=paste0(scriptpath,"/estimateG1/g12diffL.csv"))
  write.csv(retratioH, file=paste0(scriptpath,"/estimateG1/g12ratioH.csv"))
  write.csv(retratioL, file=paste0(scriptpath,"/estimateG1/g12ratioL.csv"))
  
  alen2 <- length(retratioL)
  compareretdiff <- data.frame(matrix(0,alen,alen2))
  compareretratio <- data.frame(matrix(0,alen,alen2))
  for(i in 1:alen){
    for(j in 1:alen2){
      if(G1diffG2[i,j] >= retdiffL[i,j] && G1diffG2[i,j] <= retdiffH[i,j]){
        compareretdiff[i,j] <- 1
      }
      if(G1ratioG2[i,j] >= retratioL[i,j] && G1ratioG2[i,j] <= retratioH[i,j]){
        compareretratio[i,j] <- 1
      }
    }
  }
  print (compareretdiff)
  print (compareretratio)
  write.csv(compareretdiff, file=paste0(scriptpath,"/estimateG1/compareretdiff.csv"))
  write.csv(compareretratio, file=paste0(scriptpath,"/estimateG1/compareretratiolinearfit.csv"))
}


getMaxR2 <- function(){
  simupath <- paste0(scriptpath,"/estimateG12/simulatedata/")
  g1path <- paste0(scriptpath, "/estimateG1/g1ratio.csv")
  #G1ratioG2 <- read.csv(g1path)
  
  alst <- list.files(simupath)
  lenG1G2 <- length(G1ratioG2)
  
  maxidx <- 1
  maxmean <- -0.001
  for(k in 1:Questionnum){
    lstmean <- seq(1.0,1.9,by=1.0/lenG1G2)
    for(i in 1:length(names(G1ratioG2))){ 
      curname <- names(G1ratioG2)[i]
      Ylst <- G1ratioG2[[curname]][1:Questionnum][-k]
      curname2 <- paste0(simupath,curname,"2/ratio.csv")
      data1 <- read.csv(curname2,header = TRUE)
      data1 <- data1[,-1]
      Xlst <- 1.0:10.0
      ncol <- length(data1)
      for(ii in 1:Questionnum){
        mean1 <- mean(unlist(data1[ii,1:ncol]))
        Xlst[ii] <- mean1
      }
      #print (Xlst)
      Xlst <- Xlst[-k]
      relation <- lm(Ylst~Xlst)
      abc <- summary(relation)$adj.r.squared
      lstmean[i] <- abc
    }
    ret1 <- paste0(k,":",mean(lstmean))
    if(mean(lstmean) > maxmean){
      maxmean <- mean(lstmean)
      maxidx <- k
    }
    print (ret1)
  }
  print (paste0("maxidx:",maxidx))
  return (maxidx)
}

##main-2 获取置信区间并比较
##

maxidxt <- getMaxR2()
t_row <- nrow(G1ratioG2)
t_col <- length(G1ratioG2)
G1ratioG2fit <- G1ratioG2
for(j in 1:t_col){
  med1 <- G1ratioG2[maxidxt,j]
  G1ratioG2fit[,j] <- G1ratioG2[,j] / med1
}
write.csv(G1ratioG2fit, file=paste0(scriptpath,"/estimateG1/g1ratiofit.csv"))
G1ratioG2 <- G1ratioG2fit
getG12HLinterval()
