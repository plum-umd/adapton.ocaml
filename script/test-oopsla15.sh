# output file
FILE_PRE=../out

# test runner
TESTER=../experiments.native

# these loops should be arranged in increasing priority for the variable
# and/or increasing chance to fail
# the items for the variable should be arranged in decreasing priority
# this allows the benchmark file to begin with the most important information
# in case it has to be stopped, or you'd like to peak in at the progress

# FILE=$FILE_PRE/list_filter.csv
# #len - length of the list, must be a multiple of --num-changes, which has a default of 10
# for len in 100000; do
#   #samp - sample number, is the random seed that is used to create the initial data to be modified
#   for samp in 1 2 3 4 5 6 7; do
#     #gran- granularity, average articulation chunk size is 2^gran 
#     for gran in 0; do
#       #dem - amount of the list that is demended
#       for dem in 0.000000000001 20.0 100.0; do
#         #ver - adapton version that is being tested, as well as the test set.
#         for ver in name arggen lazyrecalc; do 
#           #change - particular change that is done to the list, default is all
#           for change in "--0 --id --rr --bs"; do
#             fullver=List_filter_$ver
#             echo "Running: $fullver @ $len @ $samp"
#             args1="--sample-num $samp --n $len --demand $dem"
#             args2="--experiment $fullver --gran $gran"
#             args3="--num-changes 10" # --0 $change" #one change type at a time, on new copy of list
#             args4= #"--test-flags" #check for correctness
#             $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
#           done
#         done
#       done
#     done
#   done
# done

# FILE=$FILE_PRE/list_map.csv
# #len - length of the list, must be a multiple of --num-changes, which has a default of 10
# for len in 100000; do
#   #samp - sample number, is the random seed that is used to create the initial data to be modified
#   for samp in 1 2 3 4 5 6 7; do
#     #gran- granularity, average articulation chunk size is 2^gran 
#     for gran in 0; do
#       #dem - amount of the list that is demended
#       for dem in 0.000000000001 20.0 100.0; do
#         #ver - adapton version that is being tested, as well as the test set.
#         for ver in name arggen lazyrecalc; do 
#           #change - particular change that is done to the list, default is all
#           for change in "--0 --id --rr --bs"; do
#             fullver=List_map_$ver
#             echo "Running: $fullver @ $len @ $samp"
#             args1="--sample-num $samp --n $len --demand $dem"
#             args2="--experiment $fullver --gran $gran"
#             args3="--num-changes 10" # --0 $change" #one change type at a time, on new copy of list
#             args4= #"--test-flags" #check for correctness
#             $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
#           done
#         done
#       done
#     done
#   done
# done

FILE=$FILE_PRE/rope_mergesort.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 10000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 20.0 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--0 --id --rr --bs"; do
            fullver=Rope_mergesort_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10" # --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done
