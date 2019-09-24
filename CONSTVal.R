SCRIPT_PATH <- getwd()

##输入数据，为了保证程序的可读性及友好，请在此设置输入数据，不客气！！！
INPUTDATA <- "./data1/"
##输入数据的list
INPUTDATA_LIST <- "trymulti_glist.dat"

##OS系统下Mplus的路径
Mplus_CMD <- "/Applications/Mplus/mplus"
##WINDOWS系统用这个路径
Mplus_CMD <- "D:/program/mplus/Mplus.exe"

##并发处理数
NJOBS <- 8

##重复次数
NREPS <- 1000

##dat2esti1st.R 用到的输出路径
G1path_1st <- "/estimateG1"
G2path_1st <- "/estimateG2"
G12path_1st <- "/estimateG12"
G1flag <- "G1"
G2flag <- "G1"
G12flag <- "G12"

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

lhresetWd <- function(){
  comm1 <- "
     this is only a comment which you can ignore!
     这是为了记录脚本的最初路径，执行完每一个R脚本之后进行
     一次reset确保能找到数据和模型！谢谢！
  "
  setwd(SCRIPT_PATH)
}

