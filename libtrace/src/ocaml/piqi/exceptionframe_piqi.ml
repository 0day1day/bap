module rec Exceptionframe_piqi :
             sig
               type uint64 = int64
               
               type thread_id = Exceptionframe_piqi.uint64
               
               type address = Exceptionframe_piqi.uint64
               
               type bit_length = int
               
               type taint_id = Exceptionframe_piqi.uint64
               
               type exception_number = Exceptionframe_piqi.uint64
               
               type exception_frame = Exception_frame.t
               
             end = Exceptionframe_piqi
and
  Exception_frame :
    sig
      type t =
        { mutable exception_number : Exceptionframe_piqi.exception_number;
          mutable thread_id : Exceptionframe_piqi.thread_id option;
          mutable from_addr : Exceptionframe_piqi.address;
          mutable to_addr : Exceptionframe_piqi.address
        }
      
    end = Exception_frame
  
include Exceptionframe_piqi
  
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
and parse_exception_frame x =
  let x = Piqirun.parse_record x in
  let (_exception_number, x) =
    Piqirun.parse_required_field 1 parse_exception_number x in
  let (_thread_id, x) = Piqirun.parse_optional_field 2 parse_thread_id x in
  let (_from_addr, x) = Piqirun.parse_required_field 3 parse_address x in
  let (_to_addr, x) = Piqirun.parse_required_field 4 parse_address x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Exception_frame.exception_number = _exception_number;
       Exception_frame.thread_id = _thread_id;
       Exception_frame.from_addr = _from_addr;
       Exception_frame.to_addr = _to_addr;
     })
  
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
and gen__exception_frame code x =
  let _exception_number =
    Piqirun.gen_required_field 1 gen__exception_number
      x.Exception_frame.exception_number in
  let _thread_id =
    Piqirun.gen_optional_field 2 gen__thread_id x.Exception_frame.thread_id in
  let _from_addr =
    Piqirun.gen_required_field 3 gen__address x.Exception_frame.from_addr in
  let _to_addr =
    Piqirun.gen_required_field 4 gen__address x.Exception_frame.to_addr
  in
    Piqirun.gen_record code
      [ _exception_number; _thread_id; _from_addr; _to_addr ]
  
let gen_uint64 x = gen__uint64 (-1) x
  
let gen_int x = gen__int (-1) x
  
let gen_thread_id x = gen__thread_id (-1) x
  
let gen_address x = gen__address (-1) x
  
let gen_bit_length x = gen__bit_length (-1) x
  
let gen_taint_id x = gen__taint_id (-1) x
  
let gen_exception_number x = gen__exception_number (-1) x
  
let gen_exception_frame x = gen__exception_frame (-1) x
  
let piqi =
  [ "\226\202\2304\014exceptionframe\160\148\209H\129\248\174h\234\134\149\130\004&\130\153\170d!\218\164\238\191\004\tthread-id\210\171\158\194\006\012\218\164\238\191\004\006uint64\234\134\149\130\004$\130\153\170d\031\218\164\238\191\004\007address\210\171\158\194\006\012\218\164\238\191\004\006uint64\234\134\149\130\004$\130\153\170d\031\218\164\238\191\004\nbit-length\210\171\158\194\006\t\218\164\238\191\004\003int\234\134\149\130\004%\130\153\170d \218\164\238\191\004\btaint-id\210\171\158\194\006\012\218\164\238\191\004\006uint64\234\134\149\130\004-\130\153\170d(\218\164\238\191\004\016exception-number\210\171\158\194\006\012\218\164\238\191\004\006uint64\234\134\149\130\004\211\001\138\233\142\251\014\204\001\210\203\242$(\154\182\154\152\004\006\248\149\210\152\t\001\210\171\158\194\006\022\218\164\238\191\004\016exception-number\210\203\242$!\154\182\154\152\004\006\128\250\213\155\015\001\210\171\158\194\006\015\218\164\238\191\004\tthread-id\210\203\242$.\154\182\154\152\004\006\248\149\210\152\t\001\218\164\238\191\004\tfrom-addr\210\171\158\194\006\r\218\164\238\191\004\007address\210\203\242$,\154\182\154\152\004\006\248\149\210\152\t\001\218\164\238\191\004\007to-addr\210\171\158\194\006\r\218\164\238\191\004\007address\218\164\238\191\004\015exception-frame" ]
  

