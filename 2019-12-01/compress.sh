
dir=$(dirname $0)
cd $dir
dealfile="estimateG12 estimateG1 estimateG2"
mkdir resultdata_csv
for it in $dealfile
do
  mkdir resultdata_csv/$it
  \cp $it/*.csv resultdata_csv/$it/
done
rm -rf resultdata.zip
zip -r resultdata.zip $dealfile
rm -rf $dealfile
