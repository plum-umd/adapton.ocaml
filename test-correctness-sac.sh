experiments="\
           List_eager_map\
           List_eager_filter\
           List_map\
           List_filter\
           List_reverse\
           Rope_min\
           Rope_sum\
           Rope_mergesort\
           Rope_median\
           Quickhull
"
hline=-----------------------
hline=$hline$hline$hline$hline$hline$hline
for e in $experiments; do
    e="$e"_sac
    cmd="./experiments.byte\
       --experiment $e\
       --n 100\
       --test-flags"
    echo $hline
    echo $e
    echo $hline
    $cmd || \
        echo $0: Experiment \'$e\' Failed.  Halting further experiments.\
            && exit -1
done
