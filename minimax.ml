open TicTacToe

let possible_moves = [(0,0);(0,1);(0,2);(1,0);(1,1);(1,2);(2,0);(2,1);(2,2)]

let minimax_ref = ref (fun (game:t) (depth:int) (isMaxP:bool) -> depth)

let find_optimum game 
    (depth:int)  
    (isMaxP : bool) : (int * (int * int) ) = begin
  let start_val = if isMaxP then Int.min_int else Int.max_int in
  let best_coords = ref (List.hd possible_moves) in
  let comparator = if isMaxP then max else min in
  let best_val = 
    (List.fold_left (fun last_max coords -> begin
           match update_board game coords with
           | exception InvalidMove -> last_max
           | updated_game -> begin
               let this_v = (!minimax_ref) updated_game (depth+1) (not isMaxP) in 
               begin
                 if this_v = (comparator last_max this_v) then begin
                   best_coords := coords;
                   (comparator last_max this_v);
                 end else last_max
               end
             end
         end
       ) start_val possible_moves)
  in (best_val, !best_coords)
end

let eval_function game = 
  if win_board game then begin
    if whose_turn game = "Player 1" then 10
    else -10
  end
  else 0

let rec minimax (game:t) (depth:int) (isMaxP:bool) : int = begin
  if filled_squares game = 9 then eval_function game else begin
    let v = eval_function game in 
    if v = 0 then 
      if isMaxP then find_optimum game (depth+1) isMaxP |> fst
      else find_optimum game (depth+1) isMaxP |> fst
    else begin
      if isMaxP then v - depth else v + depth
    end
  end
end

let play_optimal_move (game:t) : (int * int) = 
  find_optimum game 0 true |> snd


let () = minimax_ref := minimax 