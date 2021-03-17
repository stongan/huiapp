#install.packages('MplusAutomation')
#install.packages('doSNOW')
#install.packages('foreach')
library(MplusAutomation)
library(doSNOW)
library(foreach)

njobs <- 20
setDefaultClusterOptions(master="localhost")
clusterobj <- makeSOCKcluster(njobs)
registerDoSNOW(clusterobj)

setwd('/Users/stongan/lihuiji/2021-03-16-code')
curpath <- getwd()
empirical_datapath<-'empirical_data.csv'
simulate_datapath <- 'generate_random_samples/'
datalist<-'datalist.dat'
item_num <-15
factor_num <-5
group_num <-2
para_num<-3*item_num+choose(factor_num,2)+2*factor_num

##Create new folders to store output files
loading_outpath <- 'loading_outputfile/'
if (!file.exists(loading_outpath)){ dir.create(loading_outpath, recursive=TRUE, showWarnings=FALSE) }
simulate_outpath <- 'loading_outputfile/simulateddata_output'
if (!file.exists(simulate_outpath)){ dir.create(simulate_outpath, recursive=TRUE, showWarnings=FALSE) }
empirical_outpath <- 'loading_outputfile/empiricaldata_output'
if (!file.exists(empirical_outpath)){ dir.create(empirical_outpath, recursive=TRUE, showWarnings=FALSE) }

#####################################################
#STEP1:Fit the confugural model to the observed data#
#####################################################
  configural <- function(lst1, fname, resfile) {
  library(MplusAutomation)
  setwd(resfile)
  retfile1 <- paste0(fname,".inp")
  
  model_var <- 'NAMES = Q1-Q15 gender;
                USEVARIABLES = Q1-Q15 gender;
                grouping=gender(1=male 2=female);'
  model_model <- 'f1 BY Q1 Q6 Q11;
                  f2 BY Q2 Q7 Q12;
                  f3 BY Q3 Q8 Q13;
                  f4 BY Q4 Q9 Q14;   
                  f5 by Q5 Q10 Q15;
    model male:
            f1 BY Q1@1 
                  Q6(LA6)
                  Q11(LA11);
            f2 BY Q2@1
                  Q7(LA7) 
                  Q12(LA12);
            f3 BY Q3@1
                  Q8(LA8) 
                  Q13(LA13);
            f4 BY Q4@1
                  Q9(LA9) 
                  Q14(LA14);   
            f5 by Q5@1
                  Q10(LA10)
                  Q15(LA15); 
            [Q1](I1);
            [Q2](I2);
            [Q3](I3);
            [Q4](I4);
            [Q5](I5);
            [Q6-Q15](IA6-IA15);
            Q1-Q15(UA1-UA15);
            f1-f5;
            [f1-f5@0];
    model female:
            f1 BY Q1@1
                  Q6(LB6)
                  Q11(LB11);
            f2 BY Q2@1
                  Q7(LB7) 
                  Q12(LB12);
            f3 BY Q3@1
                  Q8(LB8) 
                  Q13(LB13);
            f4 BY Q4@1
                  Q9(LB9) 
                  Q14(LB14);   
            f5 by Q5@1
                  Q10(LB10)
                  Q15(LB15); 
            [Q1](I1);
            [Q2](I2);
            [Q3](I3);
            [Q4](I4);
            [Q5](I5);
            [Q6-Q15](IB6-IB15);
            Q1-Q15(UB1-UB15);
            f1-f5;
            [f1-f5];'
  
  pathmodel <- mplusObject(
    TITLE = "configural model;",
    VARIABLE = model_var,
    MODEL = model_model,
    OUTPUT = "stdyx;",
    rdata = lst1)
  
  fit <- mplusModeler(pathmodel, modelout = retfile1, run = 1L)
  unlink(fit$results$input$data$file)
  unlink(retfile1)
  return (fit)
}

empirical_data <- read.csv(empirical_datapath, header = FALSE)
empiricalfit <- configural(empirical_data,'empirical',empirical_outpath)

####################################################################
#STEP2:Fit the configural model to each of the simulated samples   #
#      and calculate the differences in the factor loadings.       #
####################################################################
setwd(curpath)
datalist_path<-paste0(simulate_datapath,datalist)
simu_datalist <- as.matrix(read.table(datalist_path))

funclst <- c("configural")
foreach(r=1:nrow(simu_datalist), .verbose=T, .export=funclst, .combine="c") %dopar% {
#for(r in 1:nrow(simu_datalist)){
  setwd(curpath)
  simu_dataname <- simu_datalist[r]
  fileone <- paste0(simulate_datapath,simu_dataname)
  curdatacc <- read.table(fileone)
  filetmpgroup <- unlist(strsplit(simu_dataname, split="\\."))
  curfname <- filetmpgroup[1]
  configural(curdatacc, curfname, simulate_outpath)
}

getdiff <- function(fit1) {
  para_est <- fit1$parameters$unstandardized$est
  para_num2 <- length(para_est)
  para_num1 <- para_num2 / 2
  est1 <- para_est[1:para_num1]
  est2 <- para_est[(para_num1+1):(para_num2)]
  diff1 <- est1 - est2
  return (list(diff1,est1,est2))
}

dodiffcore <- function(){
  curname <- outfile_names[i]
  cur1 <- unlist(strsplit(curname, split="\\."))
  dataname <- cur1[1]
  curoutfile <- paste0(curpath,'/',fileout,'/',curname)
  cur_result <- readModels(curoutfile,what="all")
  curallret <- getdiff(cur_result)
  curdiff <- unlist(curallret[1])
  c(dataname,curdiff);
}

dodiff <- function(fileout,numpara) {
  simu_diff <- data.frame(matrix(NA,numpara,0))
  outfile_names <- list.files(fileout, pattern='*.out')
  funclst <- c("getdiff","curpath","readModels","simu_diff","fileout","dodiffcore","outfile_names")
  simu_diff <- foreach(i=1:length(outfile_names), .verbose=T, .export=funclst, .combine="cbind") %dopar% dodiffcore()
  colnames(simu_diff) <- simu_diff[c(1),]
  simu_diff <- simu_diff[c(2:nrow(simu_diff)),]
  write.csv(simu_diff, file=paste0(fileout,"/diff.csv"))
}
setwd(curpath)
dodiff(simulate_outpath,para_num)

#########################################################
#STEP3&4: Establish confidence interval and evaluate MI.#
#########################################################
empirical_results <- readModels(paste0(empirical_outpath,"/empirical.out"))
paraheader <- paste(empirical_results$parameters$unstandardized$paramHeader,empirical_results$parameters$unstandardized$param)
alst <- getdiff(empirical_results)
empirical_diff <- unlist(alst[1])

diff<-read.csv(paste0(simulate_outpath,"/diff.csv"))
diff2<-diff[,2:1001]
interval<- matrix(NA,para_num,3)
for (i in 1:para_num) {
  interval[i,1] <- quantile(diff2[i,],probs=0.025,na.rm = TRUE)
  interval[i,2] <- quantile(diff2[i,],probs=0.975,na.rm = TRUE)
  if(empirical_diff[i] >= interval[i,1] && empirical_diff[i] <= interval[i,2]){
    interval[i,3] <- 1
  }
    else{
     interval[i,3] <- 0
    }
}

empirical_and_CI<- cbind(paraheader[1:para_num],empirical_diff,interval)
colnames(empirical_and_CI)<-c("Parameter","Observed between-group difference","2.5% quantile","97.5% quantile","0-noninvariant,1-invariant")
write.csv(empirical_and_CI[1:item_num,], file=("loading_testresults.csv"))

stopCluster(clusterobj)