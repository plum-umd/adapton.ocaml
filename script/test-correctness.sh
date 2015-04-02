experiments="\
           List_reverse\
\
           List_map\
           List_filter\
           List_eager_filter\
           List_eager_map\
           Rope_min\
           Rope_sum\
           Rope_mergesort\
           Rope_median\
           Quickhull
"
hline=-----------------------
hline=$hline$hline$hline$hline$hline$hline
for e in $experiments; do
    for ver in _name _arggen _sac; do
        cmd="./experiments.byte\
            --experiment $e$ver\
            --n 100\
            --test-flags"
        echo $hline
        echo $e
        echo $hline
        $cmd || {
            echo $0: Experiment \'$e\' Failed. ;
            echo $0: Halting further experiments. ;
            echo $0: Run this to reproduce error: \`$cmd\`\ ;
            exit -1
        }
    done
done

echo $hline
echo "Success! All tests passed:"
for e in $experiments; do
    for ver in _name _arggen _sac; do
        echo "\t$e$ver";
    done
done
