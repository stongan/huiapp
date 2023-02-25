source("CONSTVal.R")
#a
#abc <- readModels("ex5.6.out")
#
#print (abc$parameters$unstandardized$est)
#print (abc$parameters$unstandardized$param)
#
#abc <- read.csv("/Users/stongan/Projects/R/huihui/estimateG12/simulatedata/trymulti_g4G12/ratio.csv")
#
#r <- nrow(abc)
#c <- length(abc)
#print (abc[1,abc[0,c]])
#n <- 10
#ac <- seq(1.0,1.9,by=1.0/n)
#print (class(ac))
#print (ac)
#print (ac[-2])

library(MplusAutomation)
a <- readModels("/Users/stongan/Projects/R/huihui/estimateG1/trymulti_g4G1.out")
b <- readModels("/Users/stongan/Projects/R/huihui/estimateG1/trymulti_g9G1.out")
c <- compareModels(a,b,show=c("diff", "pdiff", "summaries", "unique"), equalityMargin=c(param=.05, pvalue=.02), 
              sort="type", diffTest=TRUE, showNS=FALSE)
print(c)
