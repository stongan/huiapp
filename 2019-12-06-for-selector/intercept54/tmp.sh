
function t() {
  echo $@
  while [[ $# -gt 0 ]]; do
      key="$1"
      echo $key
      echo ${@:2}
      echo "+++++"
      shift
  done
}
function t2() {
readonly DT_SKIP=("2019-03-18" "2019-04-08" "2019-04-29")
  for v in ${DT_SKIP[@]}; do
                if [ "$v" == "$dt" ]
                then
                    bcon=1
                    break
                fi
  done
}
t $@
