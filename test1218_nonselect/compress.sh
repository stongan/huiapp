
dir=$(dirname $0)
cd $dir

dealfile="gendata selectreferent refestimateG1 refestimateG2 refestimateG12"
mkdir resultdata_csv
for it in $dealfile
do
  mkdir resultdata_csv/$it
  \cp $it/*.csv resultdata_csv/$it/
done
rm -rf resultdata.zip
zip -r resultdata.zip $dealfile
rm -rf $dealfile
