open Z_machine.Numbers

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

module Buffers : sig
  type t
  val create : (string * int) list -> t (* name & size *)
end

module Story : sig
  type t
  val create : Dict.t -> Objects.t -> Buffers.t -> Text.store -> Code.t -> t
end

val assemble : Zversion.t -> Story.t -> Z_machine.Mem.t
val assemble_and_disassemble : Zversion.t -> Story.t -> unit

val assemble_and_save : Zversion.t -> Story.t -> unit
