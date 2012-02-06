module rec Taintintroframe_piqi :
             sig
               type uint64 = int64
               
               type thread_id = Taintintroframe_piqi.uint64
               
               type address = Taintintroframe_piqi.uint64
               
               type bit_length = int
               
               type taint_id = Taintintroframe_piqi.uint64
               
               type exception_number = Taintintroframe_piqi.uint64
               
               type taint_intro_frame = Taint_intro_frame.t
               
               type taint_intro_list = Taintintroframe_piqi.taint_intro list
               
               type taint_intro = Taint_intro.t
               
             end = Taintintroframe_piqi
and
  Taint_intro_frame :
    sig
      type t =
        { mutable taint_intro_list : Taintintroframe_piqi.taint_intro_list
        }
      
    end = Taint_intro_frame
and
  Taint_intro :
    sig
      type t =
        { mutable addr : Taintintroframe_piqi.address;
          mutable taint_id : Taintintroframe_piqi.taint_id
        }
      
    end = Taint_intro
  
include Taintintroframe_piqi
  
let rec parse_uint64 x = Piqirun.int64_of_varint x
and packed_parse_uint64 x = Piqirun.int64_of_packed_varint x
and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x
and parse_thread_id x = parse_uint64 x
and packed_parse_thread_id x = packed_parse_uint64 x
and parse_address x = parse_uint64 x
and packed_parse_address x = packed_parse_uint64 x
and parse_bit_length x = parse_int x
and packed_parse_bit_length x = packed_parse_int x
and parse_taint_id x = parse_uint64 x
and packed_parse_taint_id x = packed_parse_uint64 x
and parse_exception_number x = parse_uint64 x
and packed_parse_exception_number x = packed_parse_uint64 x
and parse_taint_intro_frame x =
  let x = Piqirun.parse_record x in
  let (_taint_intro_list, x) =
    Piqirun.parse_required_field 1 parse_taint_intro_list x
  in
    (Piqirun.check_unparsed_fields x;
     { Taint_intro_frame.taint_intro_list = _taint_intro_list; })
and parse_taint_intro_list x = Piqirun.parse_list parse_taint_intro x
and parse_taint_intro x =
  let x = Piqirun.parse_record x in
  let (_addr, x) = Piqirun.parse_required_field 1 parse_address x in
  let (_taint_id, x) = Piqirun.parse_required_field 2 parse_taint_id x
  in
    (Piqirun.check_unparsed_fields x;
     { Taint_intro.addr = _addr; Taint_intro.taint_id = _taint_id; })
  
let rec gen__uint64 code x = Piqirun.int64_to_varint code x
and packed_gen__uint64 x = Piqirun.int64_to_packed_varint x
and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x
and gen__thread_id code x = gen__uint64 code x
and packed_gen__thread_id x = packed_gen__uint64 x
and gen__address code x = gen__uint64 code x
and packed_gen__address x = packed_gen__uint64 x
and gen__bit_length code x = gen__int code x
and packed_gen__bit_length x = packed_gen__int x
and gen__taint_id code x = gen__uint64 code x
and packed_gen__taint_id x = packed_gen__uint64 x
and gen__exception_number code x = gen__uint64 code x
and packed_gen__exception_number x = packed_gen__uint64 x
and gen__taint_intro_frame code x =
  let _taint_intro_list =
    Piqirun.gen_required_field 1 gen__taint_intro_list
      x.Taint_intro_frame.taint_intro_list
  in Piqirun.gen_record code [ _taint_intro_list ]
and gen__taint_intro_list code x = Piqirun.gen_list gen__taint_intro code x
and gen__taint_intro code x =
  let _addr = Piqirun.gen_required_field 1 gen__address x.Taint_intro.addr in
  let _taint_id =
    Piqirun.gen_required_field 2 gen__taint_id x.Taint_intro.taint_id
  in Piqirun.gen_record code [ _addr; _taint_id ]
  
let gen_uint64 x = gen__uint64 (-1) x
  
let gen_int x = gen__int (-1) x
  
let gen_thread_id x = gen__thread_id (-1) x
  
let gen_address x = gen__address (-1) x
  
let gen_bit_length x = gen__bit_length (-1) x
  
let gen_taint_id x = gen__taint_id (-1) x
  
let gen_exception_number x = gen__exception_number (-1) x
  
let gen_taint_intro_frame x = gen__taint_intro_frame (-1) x
  
let gen_taint_intro_list x = gen__taint_intro_list (-1) x
  
let gen_taint_intro x = gen__taint_intro (-1) x
  
let piqi =
  [ "\226\202\2304\015taintintroframe\160\148\209H\129\248\174h\234\134\149\130\004&\130\153\170d!\218\164\238\191\004\tthread-id\210\171\158\194\006\012\218\164\238\191\004\006uint64\234\134\149\130\004$\130\153\170d\031\218\164\238\191\004\007address\210\171\158\194\006\012\218\164\238\191\004\006uint64\234\134\149\130\004$\130\153\170d\031\218\164\238\191\004\nbit-length\210\171\158\194\006\t\218\164\238\191\004\003int\234\134\149\130\004%\130\153\170d \218\164\238\191\004\btaint-id\210\171\158\194\006\012\218\164\238\191\004\006uint64\234\134\149\130\004-\130\153\170d(\218\164\238\191\004\016exception-number\210\171\158\194\006\012\218\164\238\191\004\006uint64\234\134\149\130\004J\138\233\142\251\014D\210\203\242$(\154\182\154\152\004\006\248\149\210\152\t\001\210\171\158\194\006\022\218\164\238\191\004\016taint-intro-list\218\164\238\191\004\017taint-intro-frame\234\134\149\130\0043\242\197\227\236\003-\218\164\238\191\004\016taint-intro-list\210\171\158\194\006\017\218\164\238\191\004\011taint-intro\234\134\149\130\004j\138\233\142\251\014d\210\203\242$)\154\182\154\152\004\006\248\149\210\152\t\001\218\164\238\191\004\004addr\210\171\158\194\006\r\218\164\238\191\004\007address\210\203\242$ \154\182\154\152\004\006\248\149\210\152\t\001\210\171\158\194\006\014\218\164\238\191\004\btaint-id\218\164\238\191\004\011taint-intro" ]
  

