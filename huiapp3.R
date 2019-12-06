library(MplusAutomation)
library(foreach)
library(doSNOW)

replications <- 1:10
njobs <- 6
setDefaultClusterOptions(master="localhost")
clusterobj <- makeSOCKcluster(njobs)
registerDoSNOW(clusterobj)

Mplus_command <- "/Applications/Mplus/mplus"

scriptpath <- getwd()
setwd(scriptpath)

interval95<-function(x,alpha){
 xbar<-mean(x)
 sdx<-sd(x)
 n<-length(x)-1
 interval_l <- xbar-qt(1-alpha/2,n-1)*sdx/sqrt(n)
 interval_h <- xbar+qt(1-alpha/2,n-1)*sdx/sqrt(n)
 return(c(interval_l,interval_h))
}
  
G1dG2Core <- function(extrapath){
  fileout <- extrapath 
  paraG12 <- readModels(fileout,recursive =TRUE, what="all")
  
  estlen <- length(paraG12[[1]]$parameters$unstandardized$est)
  cname <- rep("NA",length(names(paraG12)))
  allret <- data.frame(matrix(NA,estlen,0))
  randomlst <- c(sample(1000:100000, 10, replace = F))
  for(i in 1:length(names(paraG12))){
    curname <- names(paraG12)[i]
    cur1 <- unlist(strsplit(curname, split="\\."))
    cname[i] <- cur1[length(cur1)-1]
    estlst <- paraG12[[curname]]$parameters$unstandardized$est
    allret[cname[i]] <- as.vector(estlst)
  }
  return (allret)
}
G1diffG2 <- function(estlstnouse, f13, randnumnouse, resfile, idx){
  curwd <- paste0(scriptpath,resfile,"/simulatedata/",f13) 
  setwd(curwd)
  g1path <- paste0(curwd,"/G1_result/")
  g2path <- paste0(curwd,"/G2_result/")
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

#invoke by callEstimate
estimateCore <- function(lst1, fname, idx, f1, resfile) {
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
    SAVEDATA = rettmp,
    rdata = lst1)
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
}

callEstimate <- function(estlst, f12, randnum, resfile, idx) {
  estipath <- paste0(scriptpath,resfile,"/simulatedata/",f12)
  lstfile <- paste0(estipath,"/G12simu_",f12,"list.dat")
  
  t_outG1 <- paste0(estipath, "/G1_result")
  t_outG2 <- paste0(estipath, "/G2_result")
  if (!file.exists(t_outG1)){ dir.create(t_outG1, recursive=TRUE, showWarnings=FALSE) }
  if (!file.exists(t_outG2)){ dir.create(t_outG2, recursive=TRUE, showWarnings=FALSE) }
  
  print (f12)
  print ("f12")
  tfname <- read.table(lstfile)
  tmatrix <- as.matrix(tfname)
  
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
    estimateCore(cur2man,fileone,1,f2,t_outG1)
    estimateCore(cur2fem,fileone,1,f2,t_outG2)
  }
}

callMplus <- function(estlst, f1, randnum, resfile, idx) {
  library(MplusAutomation)
  
  curwd_t <- paste0(scriptpath,resfile,"/simulatedata/",f1) 
  if (!file.exists(curwd_t)){ dir.create(curwd_t, recursive=TRUE, showWarnings=FALSE) }
  setwd(curwd_t)
  retfile1 <- paste0(f1,"simulate",idx,".inp")
  retfile2 <- paste0(f1,"simulate",idx,"_param.dat")
  rettmp <- paste0("RESULTS = ",retfile2,";")  
  t_monte <- "NAMES = y1-y10;
    NOBS = 150 150;
    NGROUPS=2;
    nreps=10;
    seed="
  t_monte <- paste0(t_monte,randnum)
  t_monte2 <- ";REPSAVE = ALL;
    save = G12simu_"
  t_monte3 <- "*.dat;"
  t_monte <- paste0(t_monte,t_monte2,f1,t_monte3)

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
  pathmodel <- mplusObject(
    TITLE = "data generation based on estimates of G12;",
    MONTECARLO = t_monte,
    MODELPOPULATION = t_popu,
    OUTPUT= "TECH9;")
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
}

callfunc <- function(curpath, idx) {
  
  fileout <- paste0(getwd(),curpath) 
  paraG12 <- readModels(fileout,recursive =TRUE, what="all")
  scriptpath <- scriptpath
  estlen <- length(paraG12[[1]]$parameters$unstandardized$est)
  cname <- rep("NA",length(names(paraG12)))
  allret <- data.frame(matrix(NA,estlen,0))
  randomlst <- c(sample(1000:100000, 10, replace = F))
 
  funclst <- c("callMplus","callEstimate","estimateCore","G1diffG2","G1dG2Core")
  foreach(i=1:length(names(paraG12)), .verbose=T, .export=funclst, .combine="c") %dopar% {
    setwd(scriptpath)
    curname <- names(paraG12)[i]
    cur1 <- unlist(strsplit(curname, split="\\."))
    cname[i] <- cur1[length(cur1)-1]
    estlst <- paraG12[[curname]]$parameters$unstandardized$est
    allret[cname[i]] <- as.vector(estlst)
    callMplus(estlst, cname[i], randomlst[i], curpath, idx)
    callEstimate(estlst, cname[i], randomlst[i], curpath, idx)
    G1diffG2(estlst, cname[i], randomlst[i], curpath, idx)
  }
  
  retfile <- paste0(fileout,"/param", idx, ".csv")
  write.csv(allret, file=retfile)

}

callfunc("/estimateG12", "G12")

stopCluster(clusterobj)