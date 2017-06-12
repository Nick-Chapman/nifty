open Assemble

val start : 
  ?dict:Dict.t ->
  ?objects:Objects.t -> 
  ?buffers:Buffers.t -> 
  Text.store -> 
  Assemble.Code.t ->
  [ `enter of string -> unit ]

val run : 
  ?dict:Dict.t ->
  ?objects:Objects.t -> 
  ?buffers:Buffers.t -> 
  Text.store -> 
  Assemble.Code.t ->
  unit

val dis : 
  ?dict:Dict.t ->
  ?objects:Objects.t -> 
  ?buffers:Buffers.t -> 
  Text.store -> 
  Assemble.Code.t ->
  unit

val rund : 
  ?dict:Dict.t ->
  ?objects:Objects.t -> 
  ?buffers:Buffers.t -> 
  Text.store -> 
  Assemble.Code.t ->
  unit
