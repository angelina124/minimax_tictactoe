type square_t
type t

exception InvalidMove

val init_game : t
val reset_board : t -> t
val update_board : t -> int * int -> t
val update_score : t -> string -> t
val win_board: t -> bool

val get_p1_score : t -> int
val get_p2_score : t -> int
val filled_squares : t -> int
val whose_turn : t -> string
val who_won : t -> string

val print_board : t -> unit

