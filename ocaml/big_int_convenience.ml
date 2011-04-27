(** Convenience functions for Big_ints *)

open Big_int

let bi0 = big_int_of_int 0
let bi1 = big_int_of_int 1
let bi2 = big_int_of_int 2
let bi3 = big_int_of_int 3
let bi4 = big_int_of_int 4
let bi5 = big_int_of_int 5
let bi6 = big_int_of_int 6
let bi7 = big_int_of_int 7
let bi8 = big_int_of_int 8
let bi9 = big_int_of_int 9
let bim1 = big_int_of_int (-1)
let bim2 = big_int_of_int (-2)
let bim3 = big_int_of_int (-3)
let bim4 = big_int_of_int (-4)
let bim5 = big_int_of_int (-5)
let bim6 = big_int_of_int (-6)
let bim7 = big_int_of_int (-7)
let bim8 = big_int_of_int (-8)
let bim9 = big_int_of_int (-9)

let biconst i = big_int_of_int i

(** Infix operator to test if two big ints are equal. *)
let (%==) bi1 bi2 = eq_big_int bi1 bi2

(** bi_is_zero bi returns true iff bi = 0 *)
let bi_is_zero bi = eq_big_int bi0 bi

(** bi_is_one bi returns true iff bi = 1 *)
let bi_is_one bi = eq_big_int bi1 bi

(** bi_is_minusone bi returns true iff bi = -1 *)
let bi_is_minusone bi = eq_big_int bim1 bi
