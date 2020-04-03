exception InvalidMove

type square_t = string
type t = {
  board: square_t list list;
  p1_score: int;
  p1_symbol: square_t;
  p2_score: int;
  p2_symbol: square_t;
  p1_turn: bool;
  filled_squares: int;
}


let init_game =
  {
    board = [["-";"-";"-"];["-";"-";"-"];["-";"-";"-"]];
    p1_score = 0;
    p1_symbol = "X";
    p2_score = 0;
    p2_symbol = "O";
    p1_turn = true;
    filled_squares = 0;
  }

let reset_board game = {
  board = [["-";"-";"-"];["-";"-";"-"];["-";"-";"-"]];
  p1_score = game.p1_score;
  p1_symbol = game.p1_symbol;
  p2_score = game.p2_score;
  p2_symbol = game.p2_symbol;
  p1_turn = not game.p1_turn;
  filled_squares = 0;
}

let update_score game winner = {
  board = game.board;
  p1_score = if winner = "Player 2" then game.p1_score + 1
    else game.p1_score;
  p1_symbol = game.p1_symbol;
  p2_score = if winner = "Player 1" then game.p2_score + 1
    else game.p2_score;
  p2_symbol = game.p2_symbol;
  p1_turn = not game.p1_turn;
  filled_squares = game.filled_squares;
}

let win_board game = 
  match game.board with
  | [[symb1;symb2;symb3];_;_]
    when symb1 = symb2 && symb2 = symb3 && symb1 <> "-" -> true
  | [_;[symb1;symb2;symb3];_]
    when symb1 = symb2 && symb2 = symb3 && symb1 <> "-" -> true
  | [_;_;[symb1;symb2;symb3]]
    when symb1 = symb2 && symb2 = symb3 && symb1 <> "-" -> true
  | [[symb1;_;_];[symb2;_;_];[symb3;_;_]] when 
      symb1 = symb2 && symb2 = symb3 && symb1 <> "-" -> true
  | [[_;symb1;_];[_;symb2;_];[_;symb3;_]] when 
      symb1 = symb2 && symb2 = symb3  && symb1 <> "-" -> true
  | [[_;_;symb1];[_;_;symb2];[_;_;symb3]] when 
      symb1 = symb2 && symb2 = symb3  && symb1 <> "-" -> true
  | [[_;_;symb1];[_;symb2;_];[symb3;_;_]] when 
      symb1 = symb2 && symb2 = symb3  && symb1 <> "-" -> true
  | [[symb1;_;_];[_;symb2;_];[_;_;symb3]] when 
      symb1 = symb2 && symb2 = symb3  && symb1 <> "-" -> true
  | _ -> false

let valid_update square row symbol = 
  if square = "-" then symbol::row 
  else raise InvalidMove

let update_board game coords =
  let rec update_square x (symbol:square_t) = 
    function
    | h::t -> if x = 0 then valid_update h t symbol 
      else h::(update_square (x-1) symbol t)
    | [] -> raise InvalidMove
  in
  let rec update_row y (symbol:square_t) = function
    | h::t -> if y = 0 then (update_square (fst coords) symbol h)::t 
      else h::(update_row (y-1) symbol t)
    | [] -> raise InvalidMove
  in {
    board = begin 
      if game.p1_turn then
        update_row (snd coords) game.p1_symbol game.board
      else update_row (snd coords) game.p2_symbol game.board
    end;
    p1_score = game.p1_score;
    p1_symbol = game.p1_symbol;
    p2_score = game.p2_score;
    p2_symbol = game.p2_symbol;
    p1_turn = not game.p1_turn;
    filled_squares = game.filled_squares+1;
  }

let whose_turn t = if t.p1_turn then "Player 1" else "Player 2"
let who_won t = if t.p1_turn then "Player 2" else "Player 1"
let get_p1_score t = t.p1_score
let get_p2_score t = t.p2_score
let filled_squares t = t.filled_squares

let print_row row = begin
  print_string "| ";
  List.iter 
    (fun square -> square ^ " " |> print_string) row;
  print_endline "| ";
end


let print_board (game:t) = 
  List.iter (fun row -> print_row row) game.board

