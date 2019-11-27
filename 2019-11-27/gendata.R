library(MplusAutomation)
Mplus_CMD <<- "/opt/mplus/8.3/mplus"
SCRIPT_PATH <- getwd()
curwd_t <- paste0(SCRIPT_PATH,"/gendata/") 
if (!file.exists(curwd_t)){ dir.create(curwd_t, recursive=TRUE, showWarnings=FALSE) }
setwd(curwd_t)
randomlst <- c(sample(1000:100000, 1, replace = F))
retfile1 <- paste0("gendata",".inp")

montestr1 <- "names= y1-y10;
  nobs="
montestr1 <- paste0(montestr1,Group1_samplesize," ",Group2_samplesize,";")
montestr1 <- paste0(montestr1,"NGROUPS=",NGroup1st,";")
montestr1 <- paste0(montestr1,"nreps=",100,";seed=")
montestr2 <- ";REPSAVE = ALL;
  save = data*.dat;"
montestr <- paste0(montestr1,randomlst,montestr2)
pathmodel <- mplusObject(
  TITLE = "data generation;",
  MONTECARLO = montestr, 
  MODELPOPULATION = 
  "f1 by
  y1*0.6 
  y2*0.6 
  y3*0.6
  y4*0.6
  y5*0.6
  y6*0.6             
  y7*0.6
  y8*0.6
  y9*0.6
  y10*0.6;            
  [y1-y10*0];
  y1*0.64
  y2*0.64
  y3*0.64
  y4*0.64
  y5*0.64
  y6*0.64
  y7*0.64
  y8*0.64
  y9*0.64
  y10*0.64;
  f1*1;
  [f1*0];
  MODEL POPULATION-g2:
  [y1*0.25]
  [y2*0.25]
  [y3*0.25];",    
  OUTPUT= "TECH9;")
fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)

