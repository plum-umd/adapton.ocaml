open Ocamlbuild_plugin;;

dispatch begin function
    | After_rules -> pdep ["link"] "linkdep" (fun param -> [param])
    | _ -> ()
end

