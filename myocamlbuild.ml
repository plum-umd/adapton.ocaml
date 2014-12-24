open Ocamlbuild_plugin;;

dispatch begin function
  | Before_options ->
      Options.use_ocamlfind := true
  | After_rules ->
      Pathname.define_context "Source/struct" ["Source/core"];
      Pathname.define_context "Source/test" ["Source/struct"; "Source/core"];
  | _ -> ()
end