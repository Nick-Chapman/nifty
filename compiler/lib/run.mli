open Compile

val start : unit Action.t In_image.t -> [ `enter of string -> unit ]
val run : unit Action.t In_image.t -> unit
val dis : unit Action.t In_image.t -> unit
val rund: unit Action.t In_image.t -> unit
