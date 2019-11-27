
dir=$(dirname $0)
cd $dir
dealfile="estimateG12 estimateG1 estimateG2"
rm -rf resultdata.zip
zip -r resultdata.zip $dealfile
rm -rf $dealfile
