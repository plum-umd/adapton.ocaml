FILE=runas2bench.csv

# files must be touched first to give a minimum set of permissions
touch $FILE
# remember to adjust this if the output data was changed (or delete it if you intend to append to the file)
echo "Random init, Version, Function type, Function arity, Eviction limit, Sheets, Changes, Log time, Run time, heap, stack, update, dirty, clean, evaluate, create, evict, destroy_refc, destroy_evict" >> $FILE

# test runner
TESTER=../../../_product/runas2.native

# these loops should be arranged in increasing priority for the variable
# the items for the variable should be arranged in decreasing priority
# this allows the benchmark file to begin with the most important information
# in case it has to be stopped, or you'd like to peak in at the progress

# any of these variables can be hard-coded into different output files if you need seperation
# any of these variables can be hard-coded and split into different scripts if you need parallelism
# you can call each script in its own process, but they may need to write to different files, just in case

# WARNING: The non-Adapton versions using multi arity operations run very slowly!
# Seperate them out before running the script.
# Current eviction policies other than none can run slowly as well.

for samp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16; do
  for chng in 10 20 30; do
    for sht in 2 4 6 8 10 12 14 16 18 20; do
      for ver in default inexact nocache nocount "fifo+rc" "lru+rc" "fifo-rc" "lru-rc"; do
        for ft in only_fun more_fun all_types more_cmp only_cmp; do
          for fa in only_binop more_binop all_arities more_multi only_multi; do
            args="--random_init $samp --adapton-module $ver --func-type $ft --func-arity $fa"
            args="$args --stats-test $sht --num-changes $chng --eviction-limit 1000"
            $TESTER $args --stats-out $FILE
          done
        done
      done
    done
  done
done
