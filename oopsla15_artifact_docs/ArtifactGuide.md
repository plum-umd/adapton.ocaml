Getting Started with Adapton's OOPSLA'15 Artifact
=================================================

1. Install a Program to run the virtual machine 
  * We used VirtualBox with default settings
  1. Go to https://www.virtualbox.org/wiki/Downloads
  2. Select the appropriate option for your platform
  3. Install the program

2. Import Adapton's .ova file [TODO include file name]
  * From Menu: File->Import Appliance...

3. Run the VM and log on
  1. Double-click the icon in the left pane
  2. A window will open with Arch Linux
    * there is no internal windowing system
  3. Hit enter or wait a few seconds to advance from the boot screen
  4. Enter the user/password: guest/guest

    >ic login: guest

    >Password: guest (text hidden)

4. Run our test script
  >[guest@ic ~]$ ./recreate-results.sh


  * Take a look at the generated tables

    >[guest@ic ~]$ less table1-results.txt

    >[guest@ic ~]$ less table2-results.txt

  * Our original paper data is in `adapton.ocaml/oopals15_data_log/`

  * To recreate our paper's data, use the script: `recreate-results.sh full`
    * If you plan on running our full test set which takes the better part of a day to run, you'll need to change settings on the VM to allow 8 GB of memory

5. Let us know if you have any trouble!

Step by step guide to Adapton's OOPSLA'15 Artifact
==================================================

Included in the VM
------------------

  >[guest@ic ~]$ ls

Our main Adapton git repo has been cloned to `adapton.ocaml/`.
  >[guest@ic ~]$ cd adapton.ocaml/

It is also available publically at https://github.com/plum-umd/adapton.ocaml
From here you can rebuild our test program `experiments.native` or make the Adapton library available for use with opam with `make opam-reload` (or `make opam-pin` the first time).

The `script/` directory contains some sample test scripts we've been using. `Sample/` and `Source/` contain our code. `out/` contains any results from scripts. `templates/` contains excel files used to visualize data. Feel free to clone the repo elsewhere to make use of them. The supplemental documentation contains a guide to the experiments that will be available publically in the `docs/` folder.

The directory `oopsla15_data_log` contains all our original results data that made it into the paper, along with the spreadsheet we used for post-processing.

  >[guest@ic adapton.ocaml]$ cd ../incremental-computation

This is a clone of our development repository. At the time of this writing we're reorganising to separate what we want to be public from what we want to stay private. When compiled as above, the executable (`imptests.native`), test script (`test.py`), and results directory (`results/`) are located here. The `imp/` directory contains the code being tested. 


Running Manual Experiments
--------------------------

The imp test script that generates data for table2 is `test.py`. You can edit the common parameters at the top:
  >[guest@ic incremental-computation]$ nano test.py

Nano has common commands listed at the bottom. Press control-x to exit and y [enter] to save your work.

The parameters at the top are set to run simpler tests that finish quickly. 'array_size' and 'intl_input' must be powers of 2. The second group of parameters are the ones used in the paper submission. This script is run as part of the test set above. You can call it as above, or add a parameter to run the longer set of tests `reproduce-results.sh full`. This will take many hours, though.

Tests can also be run individualy. We don't suggest doing this unless you want to deal with the details of how our tests work, but the following sequence will produce results:

  >[guest@ic incremental-computations]$ ./imptests.native --artlib fromscratch --test fact3 --record --fact 10000

  >[guest@ic incremental-computations]$ ./imptests.native --artlib structural --test fact3 --record --fact 10000

  >[guest@ic incremental-computations]$ ./imptests.native --artlib nominal --test fact3 --record --fact 10000

  >[guest@ic incremental-computations]$ cd results

  >[guest@ic incremental-computations]$ tail fact-begin-swap-assign.csv

What you see is the raw output file. The last three lines are the ones we just generated. They are more demanding but less thorough then the paper's results. The first floating point number in each line is the initial program run time. The second floating point number is the incremental run time. As you can see, Adapton ('structural', 'nominal') takes slightly longer for the initial program than Non-Adapton ('fromscratch'). The benefit is in the incremental change runtime, where Adapton performs far better.

The list experiments system was developed over a longer period of time so it is a little easier to use. The `docs/` directory contains a longer guide to use it. Feel free to skip to the listing of parameters to `experiments.native` to try out more options. The test script we ran above is `script/test-oopsla15.sh`. This also has two sets of options, the second being the set reported in our paper.

The following sequence will generate results:

  >[guest@ic incremental-computation]$ cd ~/adapton.ocaml

  >[guest@ic adapton.ocaml]$ ./experiment.native --experiment Rope_mergesort_lazyrecalc --0 --r --num-changes 1 --n 50000

  >[guest@ic adapton.ocaml]$ ./experiment.native --experiment Rope_mergesort_name --0 --r --num-changes 1 --n 50000

Again these tests are more demanding than in the paper, but less thorough. We see that Non-Adapton ('lazyrecalc') takes much less time to generate the initial list, but far more time to make an incremental change than Adapton ('name'). The terminal output is sufficient for our description, but the raw output can be found by:

  >[guest@ic adapton.ocaml]$ cd out/

  >[guest@ic adapton.ocaml]$ tail experiments.csv

Here each test has a line for initial construction, one for each change made, and one for final heap size. The time is the second floating point number (the first one with some decimal digits).

Deviations from our Paper's Results
-----------------------------------

All our results were from a 2.26 GHz Intel Mac Pro with 16 GB of RAM running Mac OS X 10.6.8. Our original data can be found in `adapton.ocaml/oopals15_data_log/`. We measure the time taken to perform calculations both processor intensive and memory intensive. Results may vary greatly depending on the system used. The trends should not vary as much, so we encourage reviewers to consider speedups rather than time taken.

Adapton has a great deal of overhead, both in memory and initial computation time. For this artifact, we have prepared our tests to generate a result set in a short period of time. This may not be enough to show off the benefits of Adapton in all cases. We encourage you to test manually with more demanding parameters as we have demonstrated above.


We hope this guide provides a good starting point to further explore our work!


