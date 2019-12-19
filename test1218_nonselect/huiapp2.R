library(MplusAutomation)
Mplus_command <- "/Applications/Mplus/mplus"
datapath <- "./data1/"
curpath <- getwd()
setwd(curpath)
outG1 <- paste0(curpath,"/estimateG1/")
outG2 <- paste0(curpath,"/estimateG2/")
outG12 <- paste0(curpath,"/estimateG12/")
if (!file.exists(outG1)){ dir.create(outG1, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(outG2)){ dir.create(outG2, recursive=TRUE, showWarnings=FALSE) }
if (!file.exists(outG12)){ dir.create(outG12, recursive=TRUE, showWarnings=FALSE) }

tfname <- read.table(paste(datapath,"trymulti_glist.dat",sep=""))
tmatrix <- as.matrix(tfname)

callfunc <- function(lst1, fname, idx, f1, resfile) {
  setwd(resfile)
  retfile1 <- paste0(f1,"G",idx,".inp")
  retfile2 <- paste0(f1,"G",idx,"_param.dat")
  rettmp <- paste0("RESULTS = ",retfile2,";")  
  pathmodel <- mplusObject(
   TITLE = "MplusAutomation Example - Path Model;",
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
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  print(coef(fit))
  unlink(fit$results$input$data$file)
}

#callfuuc2 <- function(lst1, fname, idx) {
##  print(paste(lst1, fname, idx))
#  print(getwd())
#}

for(r in 1:nrow(tmatrix)){
  setwd(curpath)
  filename <- tmatrix[r]
  fileone <- paste(datapath,filename,sep="")  
  cur1 <- read.table(fileone)
  cur2 <- data.frame(cur1)

  cur2man <- subset(cur2, V11==1)
  cur2fem <- subset(cur2, V11==2)
  
  filetmpgroup <- unlist(strsplit(filename, split="\\."))
  f2 <- filetmpgroup[1]
    
  #callfunc(cur2,fileone,12,f2,outG12)
  
  #callfunc(cur2man,fileone,1,f2,outG1)
  callfunc(cur2fem,fileone,2,f2,outG2)
  break
}

