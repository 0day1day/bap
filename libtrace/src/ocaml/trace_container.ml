exception TraceException of string

type frame = Frame_piqi.frame

(* XXX: Auto-pull this from C++ header file *)
let default_frames_per_toc_entry = 10000L
and default_auto_finish = false

(* Internal definitions *)
let magic_number = 7456879624156307493L
and magic_number_offset = 0L
and num_trace_frames_offset = 8L
and toc_offset_offset = 16L
and first_frame_offset = 24L

class writer ?(frames_per_toc_entry = default_frames_per_toc_entry) ?(auto_finish=default_auto_finish) filename =
  (* Open the trace file *)
  let oc = open_out_bin filename in
  (* Seek to the first frame *)
  let () = seek_out oc first_frame_offset in
object(self)

  val mutable toc = []
  val mutable num_frames = 0L
  val frames_per_toc_entry = frames_per_toc_entry
  val auto_finish = auto_finish
  val mutable is_finished = false

  method add (frame:frame) =
    let () = num_frames <- num_frames + 1 in
    if Int64.rem num_frames frames_per_toc_entry = 0 then
      (* Put a toc entry *)
      toc <- (pos_out oc) :: toc;

    (* Convert to string so we know length *)
    let s = Piqirun.to_string (Frame_piqi_ext.gen_frame `pb frame) in
    let len = Int64.of_int (String.length s) in

    (* len as 64-bit unsigned integer *)
    let len_binary =
      let output = BatIO.output_string () in
      let () = write_i64 output len in
      close_out output
    in
    assert (String.length len_binary = 8 (*64/8*));
    (* Write the length in binary *)
    let () = output_string oc len_binary in

    let old_offset = Int64.of_int (pos_out oc) in
    (* Finally write the serialized string out. *)
    let () = output_string oc s in

    (* Double-check our size. *)
    assert ((Int64.plus old_offset len)
        = (Int64.of_int (pos_out oc)));

  method finish =
    ()
end

(* class reader filename = *)
(* object(self) *)
(*   method get_num_frames = num_frames *)
(*   method seek offset = () *)
(*   method get_frame = () *)
(*   method get_frames = [get_frame] *)
(*   method end_of_trace = true *)
(* end *)
