library(MplusAutomation)
Mplus_command <- Mplus_CMD
datapath <- INPUTDATA
curpath <- SCRIPT_PATH
setwd(curpath)

#建文件夹
#outMCFA <- paste0(curpath,MCFApath_1st)
outMCFA <- paste0(curpath,"/estMCFA")
out1<-paste0(outMCFA,"/","scale_loadingref1")
out2<-paste0(outMCFA,"/","item_loadingref1")
out3<-paste0(outMCFA,"/","item_loadingref4")
out4<-paste0(outMCFA,"/","scale_loadingref4")
if (!file.exists(outMCFA)){ dir.create(outMCFA, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(out1)){ dir.create(out1, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(out2)){ dir.create(out2, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(out3)){ dir.create(out3, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(out4)){ dir.create(out4, recursive=TRUE, showWarnings=FALSE) }
##读入数据list进行遍历文件
tfname <- read.table(paste0(datapath,INPUTDATA_LIST))
tmatrix <- as.matrix(tfname)

#定义估计函数主体（configural model和metric model)
MCFAscale_ref1 <- function(lst1,f1,resfile) {
  ##设置输出模型到指定的文件夹
  setwd(resfile) 
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(f1,".inp")
  ##Mplus 运行的参数配置
  scalemodel <- mplusObject(
    TITLE = "scale-level MCFA for configural and metric model item1@1;",
    VARIABLE = "NAMES = V1-V11;
               USEVARIABLES = V1-V10;
               grouping = V11(2);",
    ANALYSIS = "model = configural metric;",
    MODEL = "f1 by V1-V10;",
    rdata = lst1)
  
  ##模型的运行
  fit <- mplusModeler(scalemodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
}

MCFAscale_ref4 <- function(lst1,f1,resfile) {
  ##设置输出模型到指定的文件夹
  setwd(resfile) 
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(f1,".inp")
  ##Mplus 运行的参数配置
  scalemodel <- mplusObject(
    TITLE = "scale-level MCFA for configural and metric model item4@1;",
    VARIABLE = "NAMES = V1-V11;
               USEVARIABLES = V1-V10;
               grouping = V11(2);",
    ANALYSIS = "model = configural metric;",
    MODEL = "f1 by V1* V2 V3 V4@1 V5-V10;",
    rdata = lst1)
  
  ##模型的运行
  fit <- mplusModeler(scalemodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
}


G2model<- function(inum) {
  g2m <- "f1 by V1@1"
  for(k in 2:Questionnum) {
    if(k!=inum){
      g2m <- paste0(g2m," V",k)
    }
  }
  g2m <- paste0(g2m,";")
  return (g2m)
} 

G2modelv2<- function(inum) {
  g2m <- "f1 by"
  for(k in 1:Questionnum) {
    if(k!=inum){
      g2m <- paste0(g2m," V",k)
      if(k==1){
        g2m <- paste0(g2m,"*")
      }else if(k==4){
        g2m <- paste0(g2m,"@1")
      }
    }
  }
  g2m <- paste0(g2m,";")
  return (g2m)
} 

#进行item level的载荷等价性检验，固定第1题（真值不等价）的载荷为1
itemloadingtest_ref1<-function(file1,p1,num,path,curret){
  setwd(path) 
  retfile1 <- paste0(p1,"_fix",num,".inp")
  g2define<-paste0(curret,"[V1-V10*];")
  ##Mplus 运行的参数配置

  print (g2define)
  def0 <- "f1 by V1@1 V2 V3 V4 V5 V6 V7 V8 V9 V10;
             [f1@0];
            model G2:"
  modeldefine <- paste0(def0,g2define)
  scalemodel <- mplusObject(
    TITLE = "item-level MCFA for factor loading invariance test;",
    VARIABLE = "NAMES = V1-V11;
               USEVARIABLES = V1-V10;
               grouping = V11(1=G1 2=G2);",
    MODEL = modeldefine,
    OUTPUT = "tech1;",
    rdata = file1)
  
  ##模型的运行
  fit <- mplusModeler(scalemodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
}


#进行item level的载荷等价性检验，固定第4题（真值等价）的载荷为1
itemloadingtest_ref4<-function(file1,p1,num,path,curret){
  setwd(path) 
  retfile1 <- paste0(p1,"_fix",num,".inp")
  g2define<-paste0(curret,"[V1-V10*];")
  ##Mplus 运行的参数配置
  
  print (g2define)
  def0 <- "f1 by V1* V2 V3 V4@1 V5 V6 V7 V8 V9 V10;
  [f1@0];
  model G2:"
  modeldefine <- paste0(def0,g2define)
  scalemodel <- mplusObject(
    TITLE = "item-level MCFA for factor loading invariance test;",
    VARIABLE = "NAMES = V1-V11;
    USEVARIABLES = V1-V10;
    grouping = V11(1=G1 2=G2);",
    MODEL = modeldefine,
    OUTPUT = "tech1;",
    rdata = file1)
  
  ##模型的运行
  fit <- mplusModeler(scalemodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
}

#主逻辑
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
  filetmpgroup <- unlist(strsplit(filename, split="\\."))
  f2 <- filetmpgroup[1]
  MCFAscale_ref1(cur2,f2,out1)
  MCFAscale_ref4(cur2,f2,out4)
  
  for(l in 2:Questionnum){
    g2m<-G2model(l)
    itemloadingtest_ref1(cur2,f2,l,out2,g2m)
  }
  for(l in 1:Questionnum){
    if(l != 4){
      g2m<-G2modelv2(l)
      itemloadingtest_ref4(cur2,f2,l,out3,g2m)
    }
  }
}

fitscalelen <- 100
###以下为scale level test的拟合结果整理语句
scalefitresult<-function(outpath)
{
  fitscale <- readModels(outpath, what="all")
  fitlen<-length(fitscale[[1]]$summaries)
  rowname1 <- names(fitscale[[1]]$summaries[1,])
  cname <- rep("NA",length(names(fitscale)))
  allretC <- data.frame(matrix(NA,fitlen,0))
  allretM <- data.frame(matrix(NA,fitlen,0))
  row.names(allretC) <- rowname1
  row.names(allretM) <- rowname1
  
  for(i in 1:length(names(fitscale))){
    ##之后的函数调用设计改变路径，每个循环初始化路径
    ##取当前文件名字
    curname <- names(fitscale)[i]
    ##切割文件名字，只要后缀名之前的一段
    cur1 <- unlist(strsplit(curname, split="\\."))
    cname[i] <- cur1[length(cur1)-1]
    ##抽每个out文件的参数
    fitlst <- fitscale[[curname]]$summaries
    tmpC <- fitlst[1,]
    tmpM <- fitlst[2,]
    ##按文件名保存到矩阵，生成configural和metric model的拟合指数表格
    allretC[cname[i]] <- t(tmpC)
    allretM[cname[i]] <- t(tmpM)
  }
  
  #提取数值列并生成差值
  tmpallretC<-data.matrix(allretC[6:28,])
  tmpallretM<-data.matrix(allretM[6:28,])
  #生成空矩阵存储delt卡方检验p值和是否显著的结果
  chidiffpvalue<-matrix(NA,1,length(names(fitscale)))
  chidiffptest<-matrix(NA,1,length(names(fitscale)))
  #得到metric model和configuralnodel拟合指标差值矩阵
  allretCdiffM<-tmpallretM-tmpallretC
  
  #进行delt卡方检验，存储P值到chidiffpvalue。
  for(i in 1:length(names(fitscale))){
    tmpchidifft<-pchisq(allretCdiffM[c("ChiSqM_Value"),i], df=allretCdiffM[c("ChiSqM_DF"),i], lower.tail=F)
    chidiffpvalue[1,i]<-tmpchidifft
    if(chidiffpvalue[1,i]<0.05)
    {chidiffptest[1,i]<-0} #0是不等价，限定不成立
    else {chidiffptest[1,i]<-1} #1是等价，限定成立
  }
  
  #拼接delta卡方检验结果
  cc1 <- allretCdiffM[c("ChiSqM_Value","ChiSqM_DF","CFI","TLI","AIC","BIC","RMSEA_Estimate","SRMR"),]
  scaletest<-rbind(cc1,chidiffpvalue,chidiffptest)
  #重命名
  rownames(scaletest)<-c("ChiSqM_Value","ChiSqM_DF","CFI","TLI","AIC","BIC","RMSEA_Estimate","SRMR","chidiffpvalue","chidiffptest")
  percentinvariance<-sum(scaletest[c("chidiffptest"),]==1)/length(names(fitscale))*100
  percentvariance<-sum(scaletest[c("chidiffptest"),]==0)/length(names(fitscale))*100
  #将configuralmodel、metric model和delta卡方检验等结果存入csv文件
  write.csv(allretC,file=paste0(outpath,"/configuralsummary.csv"))
  write.csv(allretM,file=paste0(outpath,"/metricsummary.csv"))
  write.csv(scaletest,file=paste0(outpath,"/scaleleveltest.csv"))
  fitscalelen <- length(names(fitscale))
  return(allretC)
}

allretCref1<-scalefitresult(out1)
allretCref4<-scalefitresult(out4)
fitscalelen <- 100

##以下为test-level test拟合结果整理语句
getdiffCHI <-function(tobecheck, tobase, toutpath, fixname){
  #生成空矩阵存储delt卡方检验p值和是否显著的结果
  tchidiffpvalue<-matrix(NA,1,fitscalelen)
  tchidiffptest<-matrix(NA,1,fitscalelen)
  #得到metric model和configuralnodel拟合指标差值矩阵
  #print (tobecheck)
  #print (tobase)
  tallretCdiffM<-tobecheck-tobase
  
  #进行delt卡方检验，存储P值到chidiffpvalue。
  for(i in 1:fitscalelen){
    tmpchidifft<-pchisq(tallretCdiffM[c("ChiSqM_Value"),i], df=tallretCdiffM[c("ChiSqM_DF"),i], lower.tail=F)
    tchidiffpvalue[1,i]<-tmpchidifft
    if(tchidiffpvalue[1,i]<0.05)
    {   tchidiffptest[1,i]<-0} #0是不等价，限定不成立
    else{   tchidiffptest[1,i]<-1} #1是等价，限定成立
  }
  
  #拼接delta卡方检验结果
  tcc1 <- tallretCdiffM[c("ChiSqM_Value","ChiSqM_DF","CFI","TLI","AIC","BIC","RMSEA_Estimate","SRMR"),]
  titemtest<-rbind(tcc1,tchidiffpvalue,tchidiffptest)
  #重命名
  rownames(titemtest)<-c("ChiSqM_Value","ChiSqM_DF","CFI","TLI","AIC","BIC","RMSEA_Estimate","SRMR","chidiffpvalue","chidiffptest")
  tpercentinvariance<-sum(titemtest[c("chidiffptest"),]==1)/fitscalelen*100
  tpercentvariance<-sum(titemtest[c("chidiffptest"),]==0)/fitscalelen*100
  #将configuralmodel、metric model和delta卡方检验等结果存入csv文件
  write.csv(titemtest,file=paste0(toutpath,"/",fixname,"_itemtest.csv"))
  return (tpercentinvariance)
}

invariant_percent <- rep(0,10)
lihuikunle <-function(tmprefix, idx, outpath, flagh){
  tfitlen<-length(tmprefix[[1]]$summaries)
  trowname <- names(tmprefix[[1]]$summaries[1,])
  tcname <- rep("NA",length(names(tmprefix)))
  fittmp <- data.frame(matrix(NA,tfitlen,0))
  row.names(fittmp) <- trowname
  
  for(i in 1:length(names(tmprefix))){
    ##取当前文件名字
    curname2 <- names(tmprefix)[i]
    ##切割文件名字，只要后缀名之前的一段
    cur2 <- unlist(strsplit(curname2, split="\\."))
    tcname[i] <- cur2[length(cur2)-1]
    ##抽每个out文件的参数
    fitlst2 <- tmprefix[[curname2]]$summaries
    ##按文件名保存到矩阵，生成configural和metric model的拟合指数表格
    fittmp[tcname[i]] <- t(fitlst2)
  }
  fittmp_6_28 <- data.matrix(fittmp[6:28,])
  xxfixname <- paste0("fix",idx)
  t1 <- 0
  if (flagh==1){
    allretC1_6_28 <- data.matrix(allretCref1[6:28,])
    t1 <- getdiffCHI(fittmp_6_28, allretC1_6_28, outpath, xxfixname)
  }
  if (flagh==4){
    allretC4_6_28 <- data.matrix(allretCref4[6:28,])
    t1 <- getdiffCHI(fittmp_6_28, allretC4_6_28, outpath, xxfixname)
  }
  return(t1) 
}

# ##固定第1题载荷等于1时，test-level test拟合结果整理
for(i in 2:10){
  prefixi <- paste0(".*fix",i,"$")
  iref1fix <- readModels(out2,filefilter = prefixi)
  invariant_percent[i] <- lihuikunle(iref1fix, i, out2, 1)
}
print (invariant_percent)
write.csv(invariant_percent, file=paste0(out2,"/invariant_percent.csv"))

invariant_percent2 <- rep(0,10)
for(i in 1:10){
  if(i!=4){
    prefixi <- paste0(".*fix",i,"$")
    iref1fix <- readModels(out3,filefilter = prefixi)
    invariant_percent2[i] <- lihuikunle(iref1fix, i, out3, 4)
  }
}
print (invariant_percent2)
write.csv(invariant_percent2, file=paste0(out3,"/invariant_percent.csv"))

##固定第4题载荷等于1时，test-level test拟合结果整理






