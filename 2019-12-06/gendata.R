library(MplusAutomation)
Mplus_CMD <<- "/opt/mplus/8.3/mplus"
SCRIPT_PATH <- getwd()
curwd_t <- paste0(SCRIPT_PATH,"/gendata/") 
if (!file.exists(curwd_t)){ dir.create(curwd_t, recursive=TRUE, showWarnings=FALSE) }
setwd(curwd_t)
randomlst <- c(sample(1000:100000, 1, replace = F))
retfile1 <- paste0("gendata",".inp")

montestr1 <- "names= y1-y15;
  nobs="
montestr1 <- paste0(montestr1,Group1_samplesize," ",Group2_samplesize,";")
montestr1 <- paste0(montestr1,"NGROUPS=",NGroup1st,";")
montestr1 <- paste0(montestr1,"nreps=",500,";seed=")
montestr2 <- ";REPSAVE = ALL;
  save = data*.dat;"
montestr <- paste0(montestr1,randomlst,montestr2)
pathmodel <- mplusObject(
  TITLE = "data generation;",
  MONTECARLO = montestr, 
  MODELPOPULATION = 
  "f1 by y1-y5*0.6;
   f2 by y6-y10*0.6;
   f3 by y11-y15*0.6;
  [y1-y15*0];
  y1-y15*0.64;
  f1-f3*1;
  [f1-f3*0];
  f2 with f1*0.3;
  f3 with f1*0.3;
  f3 with f2*0.3;
  
  MODEL POPULATION-g2:
  [y2*0.25];
  [y7*0.25];
  [y12*0.25];",    
  OUTPUT= "TECH1;")

fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)

