#!/bin/bash
ifile="interceptcondition.csv"
dos2unix $ifile
despath="batch1206"
mkdir $despath

while read line
do
  tdestname=`echo $line | awk -F',' '{print $1}'`
  g1num=`echo $line | awk -F',' '{print int($2)}'`
  g2num=`echo $line | awk -F',' '{print int($3)}'`
  factornum=`echo $line | awk -F',' '{print int($4)}'`
  quesnum=`echo $line | awk -F',' '{print int($5)}'`
  effesize=`echo $line | awk -F',' '{print $6}'`
  ilst=`echo $line | awk -F',' '{print $7}'`

  mkdir ${despath}/${tdestname}
  \cp *R $despath/${tdestname}
  \cp *sh $despath/${tdestname}

  echo "Group1_samplesize <- $g1num" >>$despath/$tdestname/CONSTVal.R
  echo "Group2_samplesize <- $g2num" >>$despath/$tdestname/CONSTVal.R
  echo "Factornum <- $factornum" >>$despath/$tdestname/CONSTVal.R
  echo "Questionnum <- $quesnum" >>$despath/$tdestname/CONSTVal.R
  echo "EFFECTSIZE_Gendata <- $effesize" >>$despath/$tdestname/CONSTVal.R
  echo "EFFECTSIZE_lst <- \"$ilst\"" >>$despath/$tdestname/CONSTVal.R
  #cd ${despath}/${tdestname}
  #Rscript huiMain.R
  #cd ../..
  echo "$tdestname  done"
done < $ifile


