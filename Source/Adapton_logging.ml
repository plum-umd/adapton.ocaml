(* 
This module serves as the user interface to the rest of the directory structure,
  renaming and reorganizing to improve usability
The directory structure will also be available as seperate modules in the library

TODO: Thoroughly test all renamings, first-class modules are sensitive to it!
*)

include Adapton_core_logging
module Types = AdaptonTypes
module Statistics = AdaptonStatistics
module Structures = Adapton_structures
