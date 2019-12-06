
## Author: Hui Li && Jinan Pang
## Last Updated: 23 Sep 2019
## Email : 251689782@qq.com 

##定义了一些常量、输入及输出等
##工具函数等
source("CONSTVal.R")
source("util_intercept.R")
model_list <- getModelHeader_gendata(Questionnum,1,0.6)
model_header <- unlist(model_list[1])
t1<-getModelHeader2_gendata(Questionnum,3,0.3)
print (t1)

lhresetWd()
#source("gendata.R")

lhresetWd()
##从data1 -> {estimateG1,estimateG2,estimateG12}
#source("dat2esti1st.R")
#source("dat2esti1st_selectrefrent.R")
#source("dat2esti1st_selectrefrent_position.R")
#source("dat2esti1st_selectrefrent_maxmincnt.R")

lhresetWd()
##estimateG12 -> {重新生成数据及模拟及G12的diff 和ratio}
#source("regeneesti2nd.R")
#source("regeneesti2nd_selectref.R")

lhresetWd()
#传统多组比较法
#source("estMCFA.R")

lhresetWd()
##{estimateG1,2} -> {重新生成数据及模拟及G12的diff 和ratio}
##判断落在置信区间的比例以观测其的显著性
#source("confiinterval3rd.R")
#source("confiinterval3rd_divmedian.R")
#source("confiinterval3rd_divmedian_4item.R")
#source("confiinterval3rd_linearfit.R")







