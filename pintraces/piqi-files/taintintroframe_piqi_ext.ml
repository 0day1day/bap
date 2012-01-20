let piqi = Taintintroframe_piqi.piqi

let _ = Piqirun_ext.init_piqi piqi



let _uint64_piqtype = Piqirun_ext.find_piqtype "uint64"
let _int_piqtype = Piqirun_ext.find_piqtype "int"
let _thread_id_piqtype = Piqirun_ext.find_piqtype "taintintroframe/thread-id"
let _address_piqtype = Piqirun_ext.find_piqtype "taintintroframe/address"
let _bit_length_piqtype = Piqirun_ext.find_piqtype "taintintroframe/bit-length"
let _taint_id_piqtype = Piqirun_ext.find_piqtype "taintintroframe/taint-id"
let _exception_number_piqtype = Piqirun_ext.find_piqtype "taintintroframe/exception-number"
let _taint_intro_frame_piqtype = Piqirun_ext.find_piqtype "taintintroframe/taint-intro-frame"
let _taint_intro_list_piqtype = Piqirun_ext.find_piqtype "taintintroframe/taint-intro-list"
let _taint_intro_piqtype = Piqirun_ext.find_piqtype "taintintroframe/taint-intro"


let parse_uint64 x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _uint64_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_uint64 buf
let parse_int x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _int_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_int buf
let parse_thread_id x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _thread_id_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_thread_id buf
let parse_address x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _address_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_address buf
let parse_bit_length x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _bit_length_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_bit_length buf
let parse_taint_id x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _taint_id_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_taint_id buf
let parse_exception_number x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _exception_number_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_exception_number buf
let parse_taint_intro_frame x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _taint_intro_frame_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_taint_intro_frame buf
let parse_taint_intro_list x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _taint_intro_list_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_taint_intro_list buf
let parse_taint_intro x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _taint_intro_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Taintintroframe_piqi.parse_taint_intro buf


let gen_uint64 ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_uint64 x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _uint64_piqtype `pb format x_pb ?opts
let gen_int ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_int x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _int_piqtype `pb format x_pb ?opts
let gen_thread_id ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_thread_id x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _thread_id_piqtype `pb format x_pb ?opts
let gen_address ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_address x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _address_piqtype `pb format x_pb ?opts
let gen_bit_length ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_bit_length x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _bit_length_piqtype `pb format x_pb ?opts
let gen_taint_id ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_taint_id x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _taint_id_piqtype `pb format x_pb ?opts
let gen_exception_number ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_exception_number x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _exception_number_piqtype `pb format x_pb ?opts
let gen_taint_intro_frame ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_taint_intro_frame x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _taint_intro_frame_piqtype `pb format x_pb ?opts
let gen_taint_intro_list ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_taint_intro_list x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _taint_intro_list_piqtype `pb format x_pb ?opts
let gen_taint_intro ?opts x (format :Piqirun_ext.output_format) = let buf =  Taintintroframe_piqi.gen_taint_intro x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _taint_intro_piqtype `pb format x_pb ?opts


let print_uint64 x = Pervasives.print_endline (gen_uint64 x `piq)
 let prerr_uint64 x = Pervasives.prerr_endline (gen_uint64 x `piq)
let print_int x = Pervasives.print_endline (gen_int x `piq)
 let prerr_int x = Pervasives.prerr_endline (gen_int x `piq)
let print_thread_id x = Pervasives.print_endline (gen_thread_id x `piq)
 let prerr_thread_id x = Pervasives.prerr_endline (gen_thread_id x `piq)
let print_address x = Pervasives.print_endline (gen_address x `piq)
 let prerr_address x = Pervasives.prerr_endline (gen_address x `piq)
let print_bit_length x = Pervasives.print_endline (gen_bit_length x `piq)
 let prerr_bit_length x = Pervasives.prerr_endline (gen_bit_length x `piq)
let print_taint_id x = Pervasives.print_endline (gen_taint_id x `piq)
 let prerr_taint_id x = Pervasives.prerr_endline (gen_taint_id x `piq)
let print_exception_number x = Pervasives.print_endline (gen_exception_number x `piq)
 let prerr_exception_number x = Pervasives.prerr_endline (gen_exception_number x `piq)
let print_taint_intro_frame x = Pervasives.print_endline (gen_taint_intro_frame x `piq)
 let prerr_taint_intro_frame x = Pervasives.prerr_endline (gen_taint_intro_frame x `piq)
let print_taint_intro_list x = Pervasives.print_endline (gen_taint_intro_list x `piq)
 let prerr_taint_intro_list x = Pervasives.prerr_endline (gen_taint_intro_list x `piq)
let print_taint_intro x = Pervasives.print_endline (gen_taint_intro x `piq)
 let prerr_taint_intro x = Pervasives.prerr_endline (gen_taint_intro x `piq)


