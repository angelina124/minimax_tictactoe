open TicTacToe
open Minimax

exception OutofBounds

let play_game_ref = ref (fun (y:bool) (x:t) -> ())

let is_valid n = if n > -1 && n < 3 then n else raise OutofBounds

let rec get_coords game = begin
  print_endline "Enter the x coordinate of your marker:";
  match read_int() |> is_valid with
  | exception Failure x -> begin
      print_endline "Oops! That was an invalid coordinate!";
      get_coords game;  
    end
  | exception (OutofBounds) -> begin
      print_endline "Oops! That was an invalid coordinate!";
      get_coords game;  
    end
  | x -> begin 
      print_endline "Enter the y coordinate of your marker:";
      match read_int() |> is_valid with
      | exception (OutofBounds) -> begin
          print_endline "Oops! That was an invalid coordinate!";
          get_coords game;  
        end
      | y -> (x,y)
    end
end

let quit_game game = begin
  let p1_score = get_p1_score game |> string_of_int in
  print_endline ("Player 1: " ^ p1_score);
  let p2_score = get_p2_score game |> string_of_int in
  print_endline ("Player 2: " ^ p2_score);
end

let update game tie = begin
  if tie then begin
    print_endline "Tie!";
    game;
  end
  else
    let winner = who_won game in
    print_endline (winner^ " wins!");
    update_score game winner;
end

let end_game game (tie:bool) : unit = begin
  print_board game;
  let updated_game = update game tie in begin
    print_endline "Play again? y/n";
    match read_line() with
    | "y" -> let new_game = reset_board updated_game in 
      (!play_game_ref) false new_game
    | _ -> (!play_game_ref) true updated_game
  end
end


let rec play_move game : unit = begin
  print_board game;
  if filled_squares game = 9 then
    end_game game true 
  else begin
    let coords = if whose_turn game = "Player 2" then 
        play_optimal_move game 
      else get_coords game in 
    begin
      print_endline ((fst coords |> string_of_int) ^ ", " ^ (snd coords |> string_of_int));
      match (update_board game coords) with
      | exception InvalidMove -> begin
          print_endline "Oops! That was an invalid move! \n
Perhaps the square was already taken?"; 
          play_move game;
        end
      | updated_game -> begin
          let did_win = win_board updated_game in
          if did_win then end_game updated_game false else
            play_move updated_game
        end
    end
  end
end


let play_game (should_quit:bool) (game:TicTacToe.t) =
  if should_quit then quit_game game 
  else begin
    print_endline "Welcome to my awesome tic tac toe game!!";
    print_endline "The top left corner is at coordinates (0,0) btw";
    play_move game;
  end

let () = play_game_ref := play_game

let main () = play_game false init_game

let () = main ()