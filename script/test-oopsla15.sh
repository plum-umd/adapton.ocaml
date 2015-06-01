# output file
FILE_PRE=../out

# test runner
TESTER=../experiments.native

FILE=$FILE_PRE/list_filter.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 100000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=List_filter_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/list_map.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 100000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=List_map_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/list_reverse.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 100000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=List_reverse_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/rope_min.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 100000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=Rope_min_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/rope_sum.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 100000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=Rope_sum_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/rope_mergesort.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 10000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=Rope_mergesort_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/rope_median.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 10000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=Rope_median_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/list_eager_filter.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen eagernoninc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=List_eager_filter_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/list_eager_map.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name arggen eagernoninc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=List_eager_map_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

FILE=$FILE_PRE/quickhull.csv
#len - length of the list, must be a multiple of --num-changes, which has a default of 10
for len in 10000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in name lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=Quickhull_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

#for Quickhull_arggen, we need to run at a lower length

for len in 1000; do
  #samp - sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #gran- granularity, average articulation chunk size is 2^gran 
    for gran in 0; do
      #dem - amount of the list that is demended
      for dem in 0.000000000001 100.0; do
        #ver - adapton version that is being tested, as well as the test set.
        for ver in arggen lazyrecalc; do 
          #change - particular change that is done to the list, default is all
          for change in "--id" "--rr"; do
            fullver=Quickhull_$ver
            echo "Running: $fullver @ $len @ $samp"
            args1="--sample-num $samp --n $len --demand $dem"
            args2="--experiment $fullver --gran $gran"
            args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
            args4= #"--test-flags" #check for correctness
            $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
          done
        done
      done
    done
  done
done

# This script takes much longer to run, and generates data
# for a plot that was not used in the OOPSLA submission

# FILE=$FILE_PRE/mergesort_linechart.csv
# #len - length of the list, must be a multiple of --num-changes, which has a default of 10
# for len in 10000 20000 30000 40000 50000 60000 70000 80000 90000 100000; do
#   #samp - sample number, is the random seed that is used to create the initial data to be modified
#   for samp in 1 2 3 4 5 6 7; do
#     #gran- granularity, average articulation chunk size is 2^gran 
#     for gran in 0; do
#       #dem - amount of the list that is demended
#       for dem in 100.0; do
#         #ver - adapton version that is being tested, as well as the test set.
#         for ver in lazyrecalc name; do 
#           #change - particular change that is done to the list, default is all
#           for change in "--id"; do
#             fullver=Rope_mergesort_$ver
#             echo "Running: $fullver @ $len @ $samp"
#             args1="--sample-num $samp --n $len --demand $dem"
#             args2="--experiment $fullver --gran $gran"
#             args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
#             args4= #"--test-flags" #check for correctness
#             $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
#           done
#         done
#       done
#     done
#   done
# done
# FILE=$FILE_PRE/mergesort_linechart.csv
# #len - length of the list, must be a multiple of --num-changes, which has a default of 10
# for len in 10000 20000 30000 40000 50000; do
#   #samp - sample number, is the random seed that is used to create the initial data to be modified
#   for samp in 1 2 3 4 5 6 7; do
#     #gran- granularity, average articulation chunk size is 2^gran 
#     for gran in 0; do
#       #dem - amount of the list that is demended
#       for dem in 100.0; do
#         #ver - adapton version that is being tested, as well as the test set.
#         for ver in arggen; do 
#           #change - particular change that is done to the list, default is all
#           for change in "--id"; do
#             fullver=Rope_mergesort_$ver
#             echo "Running: $fullver @ $len @ $samp"
#             args1="--sample-num $samp --n $len --demand $dem"
#             args2="--experiment $fullver --gran $gran"
#             args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
#             args4= #"--test-flags" #check for correctness
#             $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
#           done
#         done
#       done
#     done
#   done
# done
# FILE=$FILE_PRE/mergesort_linechart_gran.csv
# #len - length of the list, must be a multiple of --num-changes, which has a default of 10
# for len in 10000 20000 30000 40000 50000 60000 70000 80000 90000 100000; do
#   #samp - sample number, is the random seed that is used to create the initial data to be modified
#   for samp in 1 2 3 4 5 6 7; do
#     #gran- granularity, average articulation chunk size is 2^gran 
#     for gran in 5; do
#       #dem - amount of the list that is demended
#       for dem in 100.0; do
#         #ver - adapton version that is being tested, as well as the test set.
#         for ver in name; do 
#           #change - particular change that is done to the list, default is all
#           for change in "--id"; do
#             fullver=Rope_mergesort_$ver
#             echo "Running: $fullver @ $len @ $samp"
#             args1="--sample-num $samp --n $len --demand $dem"
#             args2="--experiment $fullver --gran $gran"
#             args3="--num-changes 10 --0 $change" #one change type at a time, on new copy of list
#             args4= #"--test-flags" #check for correctness
#             $TESTER $args1 $args2 $args3 $args4 --outfile $FILE
#           done
#         done
#       done
#     done
#   done
# done

