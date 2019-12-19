
alst="intercept1 intercept2 intercept3 intercept4 intercept5 intercept6 intercept7 intercept8 intercept9 intercept10"
alst="intercept11 intercept12 intercept13 intercept14 intercept15 intercept16 intercept17 intercept18 intercept19 intercept20"
alst="intercept22 intercept23 intercept25 intercept26 intercept28 intercept29"
alst="intercept21"
alst="intercept24 intercept27 intercept30"
alst="intercept31 intercept32"
alst="intercept34 intercept35 intercept37 intercept38 intercept39 intercept40 intercept41"
alst="intercept42 intercept43 intercept44 intercept45 intercept64 intercept65 intercept67 intercept68 intercept70 intercept71"
alst="intercept33"
alst="intercept36"
alst="intercept46 intercept47"
alst="intercept49 intercept66 intercept69"
alst="intercept56 intercept58 intercept59 intercept61 intercept62"
alst="intercept52 intercept53 intercept50"
alst="intercept48 intercept51 intercept54"
for it in $alst
do
    cd $it
    Rscript huiMain.R >log.log
    cd ..
    echo "$it done =============================================================="
done


