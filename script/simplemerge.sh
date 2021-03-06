# output file
FILE=../out/simplemerge.csv

# test runner
TESTER=../experiments.native

# these loops should be arranged in increasing priority for the variable
# and/or increasing chance to fail
# the items for the variable should be arranged in decreasing priority
# this allows the benchmark file to begin with the most important information
# in case it has to be stopped, or you'd like to peak in at the progress

#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 1000 2000 3000 4000 5000; do
  #ver - adapton version that is being tested
  for ver in lazyrecalc name; do 
    #samp - sample number, is the random seed that is used to create the initial data to be modified
    for samp in 1 2 3 4 5 6 7; do
      #fullver - test set to use
      fullver=Rope_mergesort_$ver
      echo
      echo "Running: $fullver n=$len s=$samp"
      args1="--sample-num $samp --n $len"
      args2="--experiment $fullver"
      args3="--0 --rr --id --ss"
      args4= #"--test-flags" #check for correctness
      $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
    done
  done
done
