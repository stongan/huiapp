library(MplusAutomation)
Mplus_CMD <<- "/opt/mplus/8.3/mplus"
SCRIPT_PATH <- getwd()
source("util_intercept.R")

curwd_t <- paste0(SCRIPT_PATH,"/gendata/") 
if (!file.exists(curwd_t)){ dir.create(curwd_t, recursive=TRUE, showWarnings=FALSE) }
setwd(curwd_t)
randomlst <- c(sample(1000:100000, 1, replace = F))
retfile1 <- paste0("gendata",".inp")

template1 <- "names=V1-V%d;
              nobs=%d %d;
              NGROUPS=%d;
              nreps=%d;
              seed=%d;
              REPSAVE = ALL;
              save = data*.dat;"
montestr <- sprintf(template1, Questionnum,
                               Group1_samplesize, Group2_samplesize,
			       NGroup1st, 
			       NREPS_Gendata,
                               randomlst)

model_list <- getModelHeader_gendata(Questionnum,Factornum,0.6) 
model_header <- unlist(model_list[1])

strt1 <- ""
if(Factornum==1){
  strt1 <- "f1*1;
            [f1*0];"
}else{
  templatet1 <- "f1-f%d*1;
  		[f1-f%d*0];"
  strt1 <- sprintf(templatet1, Factornum, Factornum)
}
strt2 <- getModelHeader2_gendata(Questionnum,Factornum,0.3)
strt3 <- ""
if(EFFECTSIZE_Gendata > 0){
  cur1 <- unlist(strsplit(EFFECTSIZE_lst, split=" "))
  strt3 <- "MODEL POPULATION-g2:\n"
  templatet3 <- "[V%s*%f];\n"
  for(i in 1:length(cur1)){
    tt3 <- sprintf(templatet3, cur1[i], EFFECTSIZE_Gendata)
    strt3 <- paste0(strt3,tt3) 
  }
}
template2 <- "%s
             [V1-V%d*0];
             V1-V%d*%f;
             %s
             %s
             %s
             "
MODELPOPULATIONstr <- sprintf(template2,model_header,
					Questionnum,
					Questionnum,0.64,
					strt1,
					strt2,
					strt3)

pathmodel <- mplusObject(
  TITLE = "data generation;",
  MONTECARLO = montestr, 
  MODELPOPULATION = MODELPOPULATIONstr, 
  OUTPUT= "TECH1;")

fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)

##########################
##  MODELPOPULATION = 	 #
##  "f1 by y1-y5*0.6;	 #
##   f2 by y6-y10*0.6;	 #
##   f3 by y11-y15*0.6;  #
##  [y1-y15*0];          #
##  y1-y15*0.64;	 #
##  f1-f3*1;		 #
##  [f1-f3*0];		 #
##  f2 with f1*0.3; 	 #
##  f3 with f1*0.3;	 #
##  f3 with f2*0.3;	 #
##  			 #
##  MODEL POPULATION-g2: #
##  [y2*0.25];		 #
##  [y7*0.25];		 #
##  [y12*0.25];",	 # 
##########################
