(** SSA simplifications

This uses all supported simplifications to try to optimize as much as possible.
*)


let simp_cfg cfg =
  let cfgref = ref cfg in
  let changed = ref true in
  while !changed do
    let (cfg,c1) = Sccvn.replacer ~opt:true !cfgref in
    let (cfg,c2) = Deadcode.do_dce cfg in
    cfgref := cfg;
    changed := c1 || c2
  done;
  !cfgref
