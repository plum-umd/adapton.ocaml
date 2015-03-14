open Ocamlbuild_plugin;;

dispatch begin function
  | Before_options ->
      (* avoid -use-ocamlfind *)
      Options.use_ocamlfind := true
  | After_rules ->
      (* Library packs need to be directed to the library definition and the source *)
      Pathname.define_context "Source/adapton_structures" ["Source"; "Source/adapton_core"; "Source/adapton_core_logging"];
      Pathname.define_context "Source/test" ["Source"; "Source/adapton_structures"; "Source/adapton_core"; "Source/adapton_core_logging"];
  | _ -> ()
end