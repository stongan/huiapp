
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

##这是一个BIG Project，函数命名尽可能保证唯一性，Thx
geneEst1st <- function(lst1, fname, idx, f1, resfile) {
  ##设置输出模型到指定的文件夹
  setwd(resfile)
  ##拼接输出文件的名字及中间结果参数
  retfile1 <- paste0(f1,"G",idx,".inp")
  retfile2 <- paste0(f1,"G",idx,"_param.dat")
  ##动态模型参数1
  rettmp <- paste0("RESULTS = ",retfile2,";")  

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
   MODEL = "f1 by V1* V2-V10;
            f1@1;",
   SAVEDATA = rettmp,
   rdata = lst1)

  ##模型的运行
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
}

##当前文件的main流程，包含主逻辑
##comment: 遍历每个文件
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
    
  ##执行模型的接口，参数顺序：数据、文件路径、文件标签、不带后缀文件名、输出路径
  geneEst1st(cur2,fileone,12,f2,outG12)
  geneEst1st(cur2man,fileone,1,f2,outG1)
  geneEst1st(cur2fem,fileone,2,f2,outG2)
}

##Congratulate !!!
##You has finish first Part, Go ahead !
