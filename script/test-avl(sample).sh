FILE=ropesort.csv

# files must be touched first to give a minimum set of permissions, and creation if non-existant
touch $FILE

# the titles for the csv columns
# remember to adjust this if the output data was changed (or delete it if you intend to append to the file)
echo "Unix Time, Seed, Version, Test, Size, Mod Pos, Mod Pos %, Demand, Demand %, Time, Unit Cost, Heap, Stack, dirty, dirty %, clean, clean %, evaluate,  evaluate %, create, create %, tables" >> $FILE

# test runner
TESTER=../../../_product/experiments.native

# these loops should be arranged in increasing priority for the variable
# and/or increasing chance to fail
# the items for the variable should be arranged in decreasing priority
# this allows the benchmark file to begin with the most important information
# in case it has to be stopped, or you'd like to peak in at the progress

#len is the length of the list
for len in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000; do
  #samp, the sample number, is the random seed that is used to create the initial data to be modified
  for samp in 1 2 3 4 5 6 7; do
    #dem is the amount of the list that is demended
    for dem in 99.0; do
      #ver is the adapton version that is being tested, as well as the test set.
      for ver in name arggen; do 
        #change is the particular change that is done to the list
        #for change in "--r" "--rr" "--di" "--id" "--ss"; do
          echo $ver @ $len
          args1="--sample-num $samp --n $len --demand $dem"
          args2="--experiment AVL_of_rope_grifola_$ver"
          args3= #"--0 $change"
          $TESTER $args1 $args2 $args3 --outfile $FILE
        #done
      done
    done
  done
done
