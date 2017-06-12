
open Z_machine

val create : 
  image0:Mem.t -> 
  tracing:bool -> 
  unit -> [`enter of (string -> unit)]
