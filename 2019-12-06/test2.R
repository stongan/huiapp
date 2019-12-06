#对selerefg1做
library("MplusAutomation")
source("CONSTVal.R")
Mplus_command <- Mplus_CMD
source("util_intercept.R")

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
selerefg1<-"refestimateG1/"
dodiff1st_high(selerefg1)
 

