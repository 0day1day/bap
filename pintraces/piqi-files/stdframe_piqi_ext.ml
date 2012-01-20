let piqi = Stdframe_piqi.piqi

let _ = Piqirun_ext.init_piqi piqi



let _uint64_piqtype = Piqirun_ext.find_piqtype "uint64"
let _int_piqtype = Piqirun_ext.find_piqtype "int"
let _binary_piqtype = Piqirun_ext.find_piqtype "binary"
let _string_piqtype = Piqirun_ext.find_piqtype "string"
let _bool_piqtype = Piqirun_ext.find_piqtype "bool"
let _thread_id_piqtype = Piqirun_ext.find_piqtype "stdframe/thread-id"
let _address_piqtype = Piqirun_ext.find_piqtype "stdframe/address"
let _bit_length_piqtype = Piqirun_ext.find_piqtype "stdframe/bit-length"
let _taint_id_piqtype = Piqirun_ext.find_piqtype "stdframe/taint-id"
let _exception_number_piqtype = Piqirun_ext.find_piqtype "stdframe/exception-number"
let _std_frame_piqtype = Piqirun_ext.find_piqtype "stdframe/std-frame"
let _operand_list_piqtype = Piqirun_ext.find_piqtype "stdframe/operand-list"
let _operand_info_piqtype = Piqirun_ext.find_piqtype "stdframe/operand-info"
let _reg_operand_piqtype = Piqirun_ext.find_piqtype "stdframe/reg-operand"
let _mem_operand_piqtype = Piqirun_ext.find_piqtype "stdframe/mem-operand"
let _operand_usage_piqtype = Piqirun_ext.find_piqtype "stdframe/operand-usage"
let _taint_info_piqtype = Piqirun_ext.find_piqtype "stdframe/taint-info"


let parse_uint64 x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _uint64_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_uint64 buf
let parse_int x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _int_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_int buf
let parse_binary x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _binary_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_binary buf
let parse_string x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _string_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_string buf
let parse_bool x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _bool_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_bool buf
let parse_thread_id x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _thread_id_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_thread_id buf
let parse_address x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _address_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_address buf
let parse_bit_length x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _bit_length_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_bit_length buf
let parse_taint_id x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _taint_id_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_taint_id buf
let parse_exception_number x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _exception_number_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_exception_number buf
let parse_std_frame x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _std_frame_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_std_frame buf
let parse_operand_list x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _operand_list_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_operand_list buf
let parse_operand_info x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _operand_info_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_operand_info buf
let parse_reg_operand x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _reg_operand_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_reg_operand buf
let parse_mem_operand x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _mem_operand_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_mem_operand buf
let parse_operand_usage x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _operand_usage_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_operand_usage buf
let parse_taint_info x (format :Piqirun_ext.input_format) = let x_pb = Piqirun_ext.convert _taint_info_piqtype format `pb x in let buf = Piqirun.init_from_string x_pb in Stdframe_piqi.parse_taint_info buf


let gen_uint64 ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_uint64 x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _uint64_piqtype `pb format x_pb ?opts
let gen_int ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_int x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _int_piqtype `pb format x_pb ?opts
let gen_binary ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_binary x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _binary_piqtype `pb format x_pb ?opts
let gen_string ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_string x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _string_piqtype `pb format x_pb ?opts
let gen_bool ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_bool x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _bool_piqtype `pb format x_pb ?opts
let gen_thread_id ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_thread_id x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _thread_id_piqtype `pb format x_pb ?opts
let gen_address ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_address x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _address_piqtype `pb format x_pb ?opts
let gen_bit_length ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_bit_length x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _bit_length_piqtype `pb format x_pb ?opts
let gen_taint_id ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_taint_id x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _taint_id_piqtype `pb format x_pb ?opts
let gen_exception_number ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_exception_number x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _exception_number_piqtype `pb format x_pb ?opts
let gen_std_frame ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_std_frame x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _std_frame_piqtype `pb format x_pb ?opts
let gen_operand_list ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_operand_list x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _operand_list_piqtype `pb format x_pb ?opts
let gen_operand_info ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_operand_info x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _operand_info_piqtype `pb format x_pb ?opts
let gen_reg_operand ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_reg_operand x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _reg_operand_piqtype `pb format x_pb ?opts
let gen_mem_operand ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_mem_operand x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _mem_operand_piqtype `pb format x_pb ?opts
let gen_operand_usage ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_operand_usage x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _operand_usage_piqtype `pb format x_pb ?opts
let gen_taint_info ?opts x (format :Piqirun_ext.output_format) = let buf =  Stdframe_piqi.gen_taint_info x in let x_pb = Piqirun.to_string buf in Piqirun_ext.convert _taint_info_piqtype `pb format x_pb ?opts


let print_uint64 x = Pervasives.print_endline (gen_uint64 x `piq)
 let prerr_uint64 x = Pervasives.prerr_endline (gen_uint64 x `piq)
let print_int x = Pervasives.print_endline (gen_int x `piq)
 let prerr_int x = Pervasives.prerr_endline (gen_int x `piq)
let print_binary x = Pervasives.print_endline (gen_binary x `piq)
 let prerr_binary x = Pervasives.prerr_endline (gen_binary x `piq)
let print_string x = Pervasives.print_endline (gen_string x `piq)
 let prerr_string x = Pervasives.prerr_endline (gen_string x `piq)
let print_bool x = Pervasives.print_endline (gen_bool x `piq)
 let prerr_bool x = Pervasives.prerr_endline (gen_bool x `piq)
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
let print_std_frame x = Pervasives.print_endline (gen_std_frame x `piq)
 let prerr_std_frame x = Pervasives.prerr_endline (gen_std_frame x `piq)
let print_operand_list x = Pervasives.print_endline (gen_operand_list x `piq)
 let prerr_operand_list x = Pervasives.prerr_endline (gen_operand_list x `piq)
let print_operand_info x = Pervasives.print_endline (gen_operand_info x `piq)
 let prerr_operand_info x = Pervasives.prerr_endline (gen_operand_info x `piq)
let print_reg_operand x = Pervasives.print_endline (gen_reg_operand x `piq)
 let prerr_reg_operand x = Pervasives.prerr_endline (gen_reg_operand x `piq)
let print_mem_operand x = Pervasives.print_endline (gen_mem_operand x `piq)
 let prerr_mem_operand x = Pervasives.prerr_endline (gen_mem_operand x `piq)
let print_operand_usage x = Pervasives.print_endline (gen_operand_usage x `piq)
 let prerr_operand_usage x = Pervasives.prerr_endline (gen_operand_usage x `piq)
let print_taint_info x = Pervasives.print_endline (gen_taint_info x `piq)
 let prerr_taint_info x = Pervasives.prerr_endline (gen_taint_info x `piq)


