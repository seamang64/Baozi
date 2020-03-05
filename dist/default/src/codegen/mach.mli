type metrics = 
  { r_size: int;                (* Size of object *)
    r_align: int }              (* Address must be multiple of this *)

val int_rep : metrics           (* Integer type *)
val char_rep : metrics          (* Char type *)
val bool_rep : metrics          (* Boolean type *)
val void_rep : metrics          (* Void type *)
val addr_rep : metrics          (* All addresses *)
val proc_rep : metrics          (* Closures *)
val param_rep : metrics         (* Procedure parameters *)
val max_align : int

val param_base : int            (* +ve offset of first param from fp *)
val local_base : int            (* -ve offset of bottom of frame head *)
val stat_link : int             (* Offset of static link *)