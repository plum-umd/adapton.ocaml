
IFDEF ADAPTON_LOG THEN
type 'a ifdef = 'a
ELSE
type 'a ifdef = unit
END

(*
module Flags = struct
  let noweakref   = IFDEF ADAPTON_NOWEAKREF   THEN true ELSE false ENDIF
  let inexactdep  = IFDEF ADAPTON_INEXACTDEP  THEN true ELSE false ENDIF
  let nocachedval = IFDEF ADAPTON_NOCACHEDVAL THEN true ELSE false ENDIF

  let output out =
    Printf.fprintf out "ADAPTON_NOWEAKREF=%b ADAPTON_INEXACTDEP=%b ADAPTON_NOCACHEDVAL=%b\n"
      noweakref inexactdep nocachedval
end
*)

(*
module type LOG = sig

  type sys_hooks = {
    mutable stack_is_empty  : (unit -> bool option) ;
    mutable stack_depth     : (unit -> int option) ;
    mutable global_seqno    : (unit -> int option)  ;
  }
  type usr_hooks = {
    mutable output_graph    : (unit -> unit) ;
  }

  val sys_hooks : sys_hooks
  val usr_hooks : usr_hooks

  val log  : [`Fork|`Thunk|`Force|`ForceRet|`Eval|`EvalRet|`Dirty|`Repair] -> (unit -> 'a) -> 'a
  val more : string -> unit
end

module Log : LOG = struct

  let get_toggle =
    let toggle =
      try
        ( Sys.getenv "ADAPTON_LOG" ) <> "" (* any non-empty string means "true" *)
      with Not_found -> false
    in
    (* http://caml.inria.fr/pub/docs/manual-camlp4/manual002.html *)
    (fun () -> IFDEF ADAPTON_LOG THEN toggle ELSE ignore toggle ; false ENDIF)

  type sys_hooks = {
    mutable stack_is_empty : (unit -> bool option) ;
    mutable stack_depth    : (unit -> int option)  ;
    mutable global_seqno   : (unit -> int option)  ;
  }
  type usr_hooks = {
    mutable output_graph   : (unit -> unit) ;
  }

  let sys_hooks = {
    stack_is_empty = (fun _ -> None) ;
    stack_depth    = (fun _ -> None) ;
    global_seqno   = (fun _ -> None) ;
  }
  let usr_hooks = {
    output_graph   = (fun _ -> ())   ;
  }

  let indent () =
    let rec loop n = match n with
      | None   -> ""
      | Some n ->
          if n = 0 then "" else (" · "^(loop (Some (n-1))))
    in
    loop (sys_hooks.stack_depth ())

  let more msg =
    if get_toggle () then
      Printf.printf "%s%! " msg

  let log op body =
    if get_toggle () then begin
      if sys_hooks.stack_is_empty () = (Some true) then (
        Printf.printf "\n<o>\n%!" ;
        Printf.printf " ┃ %s%!"
          ( match sys_hooks.global_seqno () with
              | None -> ""
              | Some id -> string_of_int id ) ;
      )
      else
        ()
      ;
      let glyphs = match op with
        | `Fork     ->"╰┓┓ "
        | `Thunk    ->"╰─▢ "
        | `Force    ->"╰─▷ "
        | `ForceRet ->"◁ "
        | `Eval     ->"╰─▶ "
        | `EvalRet  ->"◀ "
        | `Dirty    ->"╰⌁⌁⌁ "
        | `Repair   ->"╰▶▶ "
      in
      Printf.printf "\n%s %s%!" (indent ()) glyphs ;
      let x = body () in
      if ( sys_hooks.stack_depth () = Some 0 ) then
        Printf.printf "\n" ;
      x
    end
    else
      ( body () )
end

include Log
*)
