SCRIPT_PATH <- getwd()

##输入数据，为了保证程序的可读性及友好，请在此设置输入数据，不客气！！！
INPUTDATA <- "./gendata/"
##输入数据的list
INPUTDATA_LIST <- "datalist.dat"

##OS系统下Mplus的路径
#Mplus_CMD <- "/Applications/Mplus/mplus"
##WINDOWS系统用这个路径
#Mplus_CMD <- "D:/program/mplus/Mplus.exe"
##LINUX
Mplus_CMD <- "/opt/mplus/8.3/mplus"

##并发处理数
NJOBS <- 50

##重复次数
NREPS <- 1000

##样本量


##dat2esti1st.R 用到的输出路径
G1path_1st <- "/estimateG1"
G2path_1st <- "/estimateG2"
G12path_1st <- "/estimateG12"

SeleRefpath_1st <- "/selectreferent"
SeleRefMinG1_1st <- "/refestimateG1"
SeleRefMinG2_1st <- "/refestimateG2"
SeleRefMinG12_1st <- "/refestimateG12"

G1flag <- "G1"
G2flag <- "G1"
G12flag <- "G12"

##confiinterval3rd_divmedian.R用的常量
##dat1st.R regen..2rd.R 均用了以下参数
Questionnum <- 15
Factornum <- 1
HasInterception <- 1
HasFactormean <- 1
NGroup1st <- 2
Group1_samplesize <- 300
Group2_samplesize <- 300

hollyTips1 <- function(){
##  comm1 <- "
##     this is only a comment which you can ignore!
##     1、字符串变量后的复制不能带空格
##     2、diff指的matrix(G1_est) - matrix(G2_est)
##     3、ratio指的matrix(G1_est) / matrix(G2_est)
##     4、MplusAutomation.readModels(target=fileone)
##          中的文件路径最后不能带斜杠（windows os)
##          osx系统无此限制
##  "
}

getFactorCorr <- function(){
  return (Factornum*(Factornum-1)/2)
}
print (getFactorCorr())

lhresetWd <- function(){
  comm1 <- "
     this is only a comment which you can ignore!
     这是为了记录脚本的最初路径，执行完每一个R脚本之后进行
     一次reset确保能找到数据和模型！谢谢！
  "
  setwd(SCRIPT_PATH)
}

