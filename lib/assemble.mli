open Z_machine.Numbers

module Dict : sig
  type t 
  val create : string list -> t 
end

module Objects : sig 
  module Ob : sig
    type t
    val create : short_name:string -> t
  end
  type t
  val create : Ob.t list -> t
end

module Code : sig
  type t
  val create : Instr.t list -> t
end

module Story : sig
  type t
  val create : Dict.t -> Objects.t -> Text.store -> Code.t -> t
end

val assemble : Zversion.t -> Story.t -> Z_machine.Mem.t
val assemble_and_disassemble : Zversion.t -> Story.t -> unit

val assemble_and_save : Zversion.t -> Story.t -> unit
