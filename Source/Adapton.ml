(* 
This module serves as the user interface to the rest of the directory structure,
  renaming and reorganizing to improve usability
The directory structure will also be available as seperate modules in the library

TODO: Thoroughly test all renamings, first-class modules are sensitive to it!
*)

include Adapton_core
module Name = Key
module Types = AdaptonTypes
module Statistics = AdaptonStatistics
module Structures = Adapton_structures

(* these use an interanl Make(), so the include above doesn't work as intended *)
module NameLib = Adapton_core.Grifola.Default.ArtLib
module StructLib = Adapton_core.Grifola.Structural.ArtLib
module ScratchLib = Adapton_core.Grifola.FromScratch.ArtLib
