# output file
FILE=../out/granularity.csv

# test runner
TESTER=../experiments.native

# these loops should be arranged in increasing priority for the variable
# and/or increasing chance to fail
# the items for the variable should be arranged in decreasing priority
# this allows the benchmark file to begin with the most important information
# in case it has to be stopped, or you'd like to peak in at the progress

#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 1000 2000; do # 2000 3000 4000 5000 6000 7000 8000 9000 10000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #dem - amount of the list that is demended
    for dem in 100.0; do
      #ver - adapton version that is being tested, as well as the test set.
      for gran in 0 1 2 3; do 
        #change - particular change that is done to the list, default is all
        #for change in "--r" "--rr" "--di" "--id" "--ss"; do
          fullver=List_mergesort_name
          echo "Running: $fullver @ n:$len @ g:$gran @ s:$samp"
          args1="--sample-num $samp --n $len --demand $dem --gran $gran"
          args2="--experiment $fullver"
          args3="--num-changes 10" # --0 $change" #one change type at a time, on new copy of list
          args4= #"--test-flags" #check for correctness
          $TESTER $args1 --experiment List_mergesort_lazyrecalc --0 $args3 $args4 --outfile $FILE
          $TESTER $args1 $args2 $args3 $args4 --outfile $FILE          
        #done
      done
    done
  done
done
