Getting Started with Adapton's OOPSLA'15 Artifact
=================================================
* For more detailed instructions see first section of Step by Step copied below

--------

1. Install a Virtual Machine
  * We used VirtualBox www.virtualbox.com with default settings

2. Import Adapton's .ova file located at [TODO include file name]
  * From Menu: File->Import Appliance...
  * 2GB of memory and 10 mins will be enough for our sample data
  * You'll need 8GB and the better part of a day to recreate our paper's data

3. Run the VM and log on
  * user/password: guest/guest

4. Compile our source files
  * This has been done already
  * To recreate everything, use the script: `adapton-make.sh`

5. Run our test script
  * This has been done already
  * To run locally, use the script: `adapton-test.sh`
  * Our original paper data: `adapton.ocaml/oopals15_data_log/`
  * Sample results from this VM:
    * table1-results.txt
    * table2-results.txt
  * To recreate our paper's data, use the script: `adapton-test full`

6. Explore the code
  * adapton.ocaml is a public git repository, cloned in the artifact
  * adapton.ocaml/Source/test/experiments.ml runs our list experiments
  * incremental-computation/imp/imptests.ml runs our imp experiments

7. Try some experiments yourself
  * See our Step by Step guide



-------
* A different take on the above, from the step by step guide, is below:

-------




Step by step guide to Adapton's OOPSLA'15 Artifact
==================================================

Supplement to Getting Started
-----------------------------

* If you don't already have a way to run a VM, we suggest [VirtualBox](https://www.virtualbox.org/wiki/Downloads)

* Download and install one of the first 4 options depending on system type
  
  * This guide was written from OSX

* Import the VM with default settings

  `File->Import Appliance...` [TODO include file name]

* Run the VM by double-clicking the icon in the left pane

* A window will open with Arch Linux

  * there is no internal windowing system
  * hit enter or wait a few seconds to advance from the boot screen

* At the login prompt, enter the user name 'guest', and the password 'guest'

  >ic login: guest
  
  >Password: guest (text hidden)

* Generate our sample test results

  >[guest@ic ~]$ ./recreate-results.sh

* Take a look at how they turned out

  >[guest@ic ~]$ less table1-results.txt

  >[guest@ic ~]$ less table2-results.txt

Let us know if you have any trouble!

