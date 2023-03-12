open! Core

val lerp_float : lf:float -> hf:float -> float ->
                 lc:float Array.t -> hc:float Array.t ->
                 float Array.t

val lerp_int : li:int -> hi:int -> int ->
               lc:float Array.t -> hc:float Array.t ->
               float Array.t

val white : float Array.t

val red : float Array.t

val orange : float Array.t

val yellow : float Array.t

val green : float Array.t

val blue : float Array.t

val light_blue : float Array.t

val purple : float Array.t

val brown : float Array.t

val gray : float Array.t

val black : float Array.t

val grayscale : float -> float Array.t
  
val colori : int -> float Array.t
