(** Convenience functions for Big_ints *)

open Big_int

let bi0 = big_int_of_int 0x0
let bi1 = big_int_of_int 0x1
let bi2 = big_int_of_int 0x2
let bi3 = big_int_of_int 0x3
let bi4 = big_int_of_int 0x4
let bi5 = big_int_of_int 0x5
let bi6 = big_int_of_int 0x6
let bi7 = big_int_of_int 0x7
let bi8 = big_int_of_int 0x8
let bi9 = big_int_of_int 0x9
let bia = big_int_of_int 0xa
let bib = big_int_of_int 0xb
let bic = big_int_of_int 0xc
let bid = big_int_of_int 0xd
let bie = big_int_of_int 0xe
let bif = big_int_of_int 0xf
let bim1 = big_int_of_int (-0x1)
let bim2 = big_int_of_int (-0x2)
let bim3 = big_int_of_int (-0x3)
let bim4 = big_int_of_int (-0x4)
let bim5 = big_int_of_int (-0x5)
let bim6 = big_int_of_int (-0x6)
let bim7 = big_int_of_int (-0x7)
let bim8 = big_int_of_int (-0x8)
let bim9 = big_int_of_int (-0x9)
let bima = big_int_of_int (-0xa)
let bimb = big_int_of_int (-0xb)
let bimc = big_int_of_int (-0xc)
let bimd = big_int_of_int (-0xd)
let bime = big_int_of_int (-0xe)
let bimf = big_int_of_int (-0xf)

let biconst i = big_int_of_int i

(** Infix operator to test if two big ints are equal. *)
let (==%) bi1 bi2 = eq_big_int bi1 bi2

(** Infix operator to test for non-equality *)
let (<>%) bi1 bi2 = not (bi1 ==% bi2)

(** Infix operator for < *)
let (<%) bi1 bi2 = lt_big_int bi1 bi2

(** Infix operator for <= *)
let (<=%) bi1 bi2 = le_big_int bi1 bi2

(** Infix operator for > *)
let (>%) bi1 bi2 = gt_big_int bi1 bi2

(** Infix operator for >= *)
let (>=%) bi1 bi2 = ge_big_int bi1 bi2

(** Infix operator for << *)
let (<<%) bi1 i2 = shift_left_big_int bi1 i2

(** Infix operator for >> *)
let (>>%) bi1 i2 = shift_right_big_int bi1 i2

(** Infix operator for $>> *)
let ($>>%) bi1 i2 = shift_right_towards_zero_big_int bi1 i2

(** Infix operator for + *)
let (+%) bi1 bi2 = add_big_int bi1 bi2

(** Infix operator for - *)
let (-%) bi1 bi2 = sub_big_int bi1 bi2

(** Infix operator for | *)
let (|%) bi1 bi2 = or_big_int bi1 bi2

(** Infix operator for & *)
let (&%) bi1 bi2 = and_big_int bi1 bi2

(** bi_is_zero bi returns true iff bi = 0 *)
let bi_is_zero bi = bi0 ==% bi

(** bi_is_one bi returns true iff bi = 1 *)
let bi_is_one bi = bi1 ==% bi

(** bi_is_minusone bi returns true iff bi = -1 *)
let bi_is_minusone bi = bim1 ==% bi
