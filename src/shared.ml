(* http://en.wikipedia.org/wiki/Find_first_set *)
let rec ffs x =
  if x = 0 then 0
  else
    let rec loop t r =
      if (x land t) = 0 then r
      else loop (t lsl 1) (r + 1)
    in loop 1 0
