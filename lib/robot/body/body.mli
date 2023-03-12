open Mat_vec
open! Core

type t = {

    joints : Joint.t list;
    
    transformation : Mat.t
  }
