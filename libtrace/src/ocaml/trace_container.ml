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

let write_i64 oc i64 =
  let output = BatIO.output_string () in
  let () = BatIO.write_i64 output i64 in
  let binary = BatIO.close_out output in
  output_string oc binary

class writer ?(frames_per_toc_entry = default_frames_per_toc_entry) ?(auto_finish=default_auto_finish) filename =
  (* Open the trace file *)
  let oc = open_out_bin filename in
  (* Seek to the first frame *)
  let () = seek_out oc (Int64.to_int first_frame_offset) in
object(self)

  val mutable toc = []
  val mutable num_frames = 0L
  val frames_per_toc_entry = frames_per_toc_entry
  val auto_finish = auto_finish
  val mutable is_finished = false

  method add (frame:frame) =
    let () = num_frames <- Int64.succ num_frames in
    if Int64.rem num_frames frames_per_toc_entry = 0L then
      (* Put a toc entry *)
      toc <- (pos_out oc) :: toc;

    (* Convert to string so we know length *)
    let s = Frame_piqi_ext.gen_frame frame `pb in
    let len = Int64.of_int (String.length s) in

    (* Write the length in binary *)
    let () = write_i64 oc len in

    let old_offset = Int64.of_int (pos_out oc) in
    (* Finally write the serialized string out. *)
    let () = output_string oc s in

    (* Double-check our size. *)
    assert ((Int64.add old_offset len)
        = (Int64.of_int (pos_out oc)));

  method finish =
    let toc_offset = Int64.of_int (pos_out oc) in
    (* Make sure the toc is the right size. *)
    let () = assert ((Int64.div num_frames frames_per_toc_entry) = Int64.of_int (List.length toc)) in
    (* Write frames per toc entry. *)
    let () = write_i64 oc frames_per_toc_entry in
    (* Write toc to file. *)
    let () = List.iter (fun offset -> write_i64 oc (Int64.of_int offset)) (List.rev toc) in
    (* Now we need to write the magic number, number of trace frames,
       and the offset of field m at the start of the trace. *)
    (* Magic number. *)
    let () = seek_out oc (Int64.to_int magic_number_offset) in
    let () = write_i64 oc magic_number in
    (* Number of trace frames. *)
    let () = seek_out oc (Int64.to_int num_trace_frames_offset) in
    let () = write_i64 oc num_frames in
    (* Offset of toc. *)
    let () = seek_out oc (Int64.to_int toc_offset_offset) in
    let () = write_i64 oc toc_offset in
    (* Finally close the final and mark us as finished. *)
    let () = close_out oc in
    is_finished <- true
end

(* class reader filename = *)
(* object(self) *)
(*   method get_num_frames = num_frames *)
(*   method seek offset = () *)
(*   method get_frame = () *)
(*   method get_frames = [get_frame] *)
(*   method end_of_trace = true *)
(* end *)
