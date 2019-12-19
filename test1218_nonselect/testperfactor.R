

###########################################################################
#下面这个是辅助函数，辅助生成选择intercept referent的模型中截距估计的部分 #
###########################################################################
selectreferent_perfactor_helper <- function(begin1, end1, fixinterceptidx){
  ret <- ""
  template1 <- "[V%d-V%d];"
  template2 <- "[V%d];[V%d-V%d];"
  template3 <- "[V%d-V%d];[V%d];"
  template4 <- "[V%d-V%d];[V%d-V%d];"
  if (begin1 == fixinterceptidx) {
    ret <- sprintf(template1, begin1+1, end1)
  } else if(end1 == fixinterceptidx) {
    ret <- sprintf(template1, begin1, end1-1)
  } else if(begin1 == fixinterceptidx-1){
    ret <- sprintf(template2, begin1, fixinterceptidx+1, end1)
  } else if(end1 == fixinterceptidx+1){
    ret <- sprintf(template3, begin1, fixinterceptidx-1, end1)
  } else {
    ret <- sprintf(template4, begin1, fixinterceptidx-1, fixinterceptidx+1, end1)
  }
  return (ret)
}
#######################################################################################################
# 下面的函数用于生成模型主体结构，该模型用于进行intercept referent的选择，在选择时，拟每个因子单独选择#
# 函数参数为题目数量、因子个数、寻找第几个因子上的referent、固定截距相等的题号（该因子的第几道题）    #
#######################################################################################################
selectreferent_perfactor<-function(cQuesNum, cFactorNum, factoridx, interceptidx){
  usevarbegin <- ((cQuesNum/cFactorNum)*(factoridx-1)+1)
  usevarend <- ((cQuesNum/cFactorNum)*factoridx)
  
  retmodel <- ""
  template1 <- "
  NAMES = V1-V%d;
  USEVARIABLES = V%d-V%d;
  grouping=V%d(1=g1 2=g2);
  "
  model_VARIABLE <- sprintf(template1, (cQuesNum+1), usevarbegin, usevarend, (cQuesNum+1))
  
  template2 <- "
  f%d by V%d-V%d;
  model g1:
  f%d by V%d-V%d(L%d-L%d);
  f%d@1;
  [f%d@0];
  [V%d@0];
  %s
  V%d-V%d;
  model g2:
  f%d by V%d-V%d(L%d-L%d);
  f%d;
  [f%d];
  [V%d@0];
  %s
  V%d-V%d;
  "
  
  freeintercept <- selectreferent_perfactor_helper(usevarbegin, usevarend, interceptidx+usevarbegin-1)
  model_MODEL <- sprintf(template2, 
                         factoridx, usevarbegin, usevarend, #model
                         factoridx, usevarbegin, usevarend, usevarbegin, usevarend,#model g1
                         factoridx,
                         factoridx,
                         interceptidx+usevarbegin-1,
                         freeintercept,
                         usevarbegin, usevarend,
                         factoridx, usevarbegin, usevarend, usevarbegin, usevarend,#model g2
                         factoridx,
                         factoridx,
                         interceptidx+usevarbegin-1,
                         freeintercept,
                         usevarbegin, usevarend)
  return (list(model_VARIABLE, model_MODEL))
}
selectreferent_perfactor_multi_helper <- function(cQuesNum, cFactorNum, interceptidxlst){
  template1 <- "[V%d@0];\n"
  ret <- ""
  len1 <- as.integer(cQuesNum/cFactorNum)
  for(i in 1:cFactorNum){
    delidx <- interceptidxlst[i]
    begin1 <- ((i-1)*len1+1)
    end1 <- len1*i
    c1 <- sprintf(template1, delidx+begin1-1)
    str1 <- selectreferent_perfactor_helper(begin1, end1, delidx+begin1-1)
    ret <- paste0(ret,c1)
    ret <- paste0(ret,str1)
  }
  return (ret)
}
getModelHeader<-function(cQuesNum, cFactorNum){
  len1 <- as.integer(cQuesNum/cFactorNum)
  ret <- ""
  ret2 <- ""
  template <- "f%d by V%d-V%d;\n"
  template2 <- "f%d by V%d-V%d(L%d-L%d);\n"
  for(i in 1:cFactorNum){
    cur <- sprintf(template, i, ((i-1)*len1+1), (i*len1))
    ret <- paste0(ret,cur)
    cur2 <- sprintf(template2, i, ((i-1)*len1+1), (i*len1), ((i-1)*len1+1), (i*len1))
    ret2 <- paste0(ret2,cur2)
  }
  return (list(ret,ret2))
}

selectreferent_perfactor_multifactor<-function(cQuesNum, cFactorNum, interceptidxlst){
  retmodel <- ""
  template1 <- "
  NAMES = V1-V%d;
  USEVARIABLES = V1-V%d;
  grouping=V%d(1=g1 2=g2);
  "
  model_VARIABLE <- sprintf(template1, (cQuesNum+1), cQuesNum, (cQuesNum+1))
  
  model_list <- getModelHeader(cQuesNum, cFactorNum)
  model_header <- unlist(model_list[1])
  model_header2 <- unlist(model_list[2])
  
  template2 <- "
  %s
  model g1:
  %s
  f1-f%d@1;
  [f1-f%d@0];
  %s
  V1-V%d;
  model g2:
  %s
  f1-f%d;
  [f1-f%d];
  %s
  V1-V%d;
  "
  model_header3 <- selectreferent_perfactor_multi_helper(cQuesNum, cFactorNum, interceptidxlst)
  model_MODEL <- sprintf(template2, 
                         model_header,
                         model_header2,
                         cFactorNum,
                         cFactorNum,
                         model_header3,
                         cQuesNum,
                         model_header2,
                         cFactorNum,
                         cFactorNum,
                         model_header3,
                         cQuesNum)
                         
  return (list(model_VARIABLE, model_MODEL))
}

selectreferent_perfactor_merge_0<-function(cQuesNum, cFactorNum, interceptidxlst){
  
  retmodel <- ""
  template1 <- "
  NAMES = V1-V%d;
  USEVARIABLES = V1-V%d;
  "
  model_VARIABLE <- sprintf(template1, (cQuesNum+1), cQuesNum)
  
  ###f1 by V1* V2-V5;
  template1_1 <- "f%d by V%d* V%d-V%d;"
  len1 <- as.integer(cQuesNum/cFactorNum)
  str1_1 <- ""
  for(i in 1:cFactorNum){
    begin1 <- ((i-1)*len1+1)
    end1 <- (i*len1)
    c1 <- sprintf(template1_1, i, begin1, (begin1+1), end1)
    str1_1 <- paste0(str1_1, c1)
  }
  
  template2_2 <- "
  f1-f%d@1;
  [f1-f%d];"
  str2_2 <- sprintf(template2_2, cFactorNum, cFactorNum)
  if(cFactorNum == 1){
    str2_2 <- "
    f1@1;
    [f1];
    "
  }
  
  str3_3 <- selectreferent_perfactor_multi_helper(cQuesNum, cFactorNum, interceptidxlst)
  
  template4_4 <- "
  %s
  %s
  %s
  V1-V%d;
  "
  model_MODEL <- sprintf(template4_4, 
                         str1_1, 
                         str2_2,
                         str3_3,
                         cQuesNum)
  
  return (list(model_VARIABLE, model_MODEL))
}
selectreferent_perfactor_merge_0(10, 1, c(2))
#selectreferent_perfactor_merge_0(30, 3, c(2,2,4))
#selectreferent_perfactor_merge_0(30, 3, c(1,5,8))
#selectreferent_perfactor_merge_0(100, 5, c(1,20,12,6,8))
#selectreferent_perfactor_merge_0(100, 5, c(6,8,19,2,1))
#selectreferent_perfactor(60, 3, 3, 4)
#selectreferent_perfactor(100, 5, 4, 12)
#selectreferent_perfactor(30, 3, 1, 2)
#selectreferent_perfactor(10, 2, 2, 5)
#selectreferent_perfactor_merge_0(10, 2, c(1,2))
#