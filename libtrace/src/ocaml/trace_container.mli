exception TraceException of string

val default_frames_per_toc_entry : int64
val default_auto_finish : bool

type frame = Frame_piqi.frame

class writer : string -> ?frames_per_toc_entry : int64 -> ?auto_finish : bool ->
object
  method add : frame
  method finish : unit
end

class reader : string ->
object
  method get_num_frames : int64
  method seek : int64
  method get_frame : frame
  method get_frames : frame list
  method end_of_trace : bool
end
