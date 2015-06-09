Getting Started with Adapton's OOPSLA'15 Artifact
=================================================

1. Install a Program to run the virtual machine 
  * We used VirtualBox with default settings
  1. Go to https://www.virtualbox.org/wiki/Downloads
  2. Select the appropriate option for your platform
  3. Install the program

2. Import Adapton's .ova file [TODO include file name]
  * From Menu: File->Import Appliance...
  * If you plan on running our full test set which takes the better part of a day to run, you'll need to change settings to allow 8 GB of memory

3. Run the VM and log on
  * there is no internal windowing system
  1. Double-click the icon in the left pane
  2. A window will open with Arch Linux
  3. Hit enter or wait a few seconds to advance from the boot screen
  4. Enter the user/password: guest/guest

4. Run our test script
  >[guest@ic ~]$ ./recreate-results.sh


  * Take a look at the generated tables

    >[guest@ic ~]$ less table1-results.txt

    >[guest@ic ~]$ less table2-results.txt

  * Our original paper data is in `adapton.ocaml/oopals15_data_log/`

  * To recreate our paper's data, use the script: `recreate-results.sh full`

5. Try some experiments yourself
  * See our Step by Step guide on the following pages

Let us know if you have any trouble!
