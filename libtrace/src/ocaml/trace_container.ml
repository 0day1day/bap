(** $Id$
    Trace container implementation.

    Bugs:
    We keep track of things as Int64's, but ML's IO only uses ints.
*)

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

let read_i64 ic =
  Printf.printf "at: %d\n" (pos_in ic);
  let s = String.create 8 in
  (* Read 8 bytes into s *)
  let () = really_input ic s 0 8 in
  Printf.printf "ok!\n";
  let input = BatIO.input_string s in
  let i = BatIO.read_i64 input in
  let () = BatIO.close_in input in
  i

(** [foldn f i n] is f (... (f (f i n) (n-1)) ...) 0 *)
let rec foldn64 ?(t=0L) f i n =
  let (-) = Int64.sub in
  match n-t with
  | 0L -> f i n
  | _ when n>t -> foldn64 ~t f (f i n) (n-1L)
  | n when n == -1L -> i (* otags has trouble with '-1L' *)
  | _ -> raise (Invalid_argument "negative index number in foldn64")

(* End helpers *)

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

class reader filename =
  let ic = open_in_bin filename in
  (* Verify magic number *)
  let () = if read_i64 ic <> magic_number then
      raise (TraceException "Magic number is incorrect") in
  (* Read number of trace frames. *)
  let num_frames = read_i64 ic in
  (* Find offset of toc. *)
  let toc_offset = read_i64 ic in
  (* Find the toc. *)
  let () = seek_in ic (Int64.to_int toc_offset) in
  (* Read number of frames per toc entry. *)
  let frames_per_toc_entry = read_i64 ic in
  (* Read each toc entry. *)
  let () = Printf.fprintf stderr "here we go\n" in
  let toc =
    let toc_rev = foldn64 ~t:1L
      (fun acc n ->
        Printf.printf "n: %Lx\n" n;
        (read_i64 ic) :: acc
      ) [] (Int64.div num_frames frames_per_toc_entry) in
    Array.of_list (List.rev toc_rev)
  in
  (* We should be at the end of the file now. *)
  let () = assert ((pos_in ic) = (in_channel_length ic)) in
  object(self)
    val mutable current_frame = 0L

    method get_num_frames = num_frames

    method seek frame_number =
      (* First, make sure the frame is in range. *)
      let () = self#check_end_of_trace_num frame_number "seek to non-existant frame" in

      (* Find the closest toc entry, if any. *)
      let toc_number = Int64.div frame_number frames_per_toc_entry in

      current_frame <- (match toc_number with
      | 0L -> let () = seek_in ic (Int64.to_int first_frame_offset) in
              0L
      | _ -> let () = seek_in ic (Int64.to_int (Array.get toc (Int64.to_int (Int64.pred toc_number)))) in
             Int64.mul toc_number frames_per_toc_entry);

      while current_frame <> frame_number do
        (* Read frame length and skip that far ahead. *)
        let frame_len = read_i64 ic in
        let () = seek_in ic (Int64.to_int (Int64.add (Int64.of_int (pos_in ic)) frame_len)) in
        current_frame <- Int64.succ current_frame
      done

    method get_frame : frame =
      let () = self#check_end_of_trace "get_frame on non-existant frame" in

      let frame_len = read_i64 ic in

      let buf = String.make (4*(Int64.to_int frame_len)) 'a' in
      for i = 0 to (String.length buf)-1 do
        Printf.printf "i: %d %x\n" i (Char.code (String.get buf i))
      done;
      (* Read the frame info buf. *)
      Printf.printf "I am at %d\n" (pos_in ic);
      let () = really_input ic buf (Int64.to_int frame_len) 0 in
      Printf.printf "parsing.. length = %Ld\n" frame_len;
      for i = 0 to (String.length buf)-1 do
        Printf.printf "i: %d %x\n" i (Char.code (String.get buf i))
      done;
      Printf.printf "\n wtf %d\n" (pos_in ic);
      let f = Frame_piqi.parse_frame (Piqirun.init_from_string buf) in
      let () = current_frame <- Int64.succ current_frame in

      f

    method get_frames num_frames =
      let () = self#check_end_of_trace "get_frame on non-existant frame" in

      (* The number of frames we copy is bounded by the number of
         frames left in the trace. *)
      let num_frames = max num_frames (Int64.sub num_frames current_frame) in

      List.rev (foldn64 (fun l _ -> self#get_frame :: l) [] num_frames)

    method end_of_trace =
      self#end_of_trace_num current_frame

    method private end_of_trace_num frame_num =
      if Int64.succ frame_num > num_frames then
        true
      else
        false

    method private check_end_of_trace_num frame_num msg =
      if self#end_of_trace_num frame_num then
        raise (TraceException msg)

    method private check_end_of_trace msg =
      if self#end_of_trace then
        raise (TraceException msg)

    initializer self#seek 0L
  end
