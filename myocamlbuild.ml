open Ocamlbuild_plugin;;

dispatch begin function
  | Before_options ->
      Options.use_ocamlfind := true
  | After_rules ->
      Pathname.define_context "Source/adapton_structures" ["Source"; "Source/adapton_core"];
      Pathname.define_context "Source/test" ["Source/adapton_structures"; "Source/adapton_core"];
  | _ -> ()
end