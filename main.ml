let width = 6
let height = 2
let length = width*height
let nbSeeds = 4


















(* The awale game state includes the board and the scores of both players *)
type awale = int array * int ref * int ref

(* Define the two players *)
type player = 
  | Player1
  | Player2

(* Initialize the awale game *)
let init_awale () : awale =
  let board = Array.make length nbSeeds in
  let score1 = ref 0 in
  let score2 = ref 0 in
  (board, score1, score2)























(* Get the player who own the square i *)
let index_to_player (i:int) = if i < width then Player1 else Player2

(* Get the other player *)
let other_player (p: player) : player =
  match p with
  |Player1 -> Player2
  |Player2 -> Player1

(* Get the next position in the awale board *)
let suivant (index: int) : int = (index + 1) mod length

(* Get the previous position in the awale board *)
let precedent (index: int) : int = (index - 1 + length) mod length

(* Give index of the last seed *)
let advance (start: int) (seed: int) : int = (start mod length) + (start / length)

(* Take seeds from the board *)
let take ((board, score1, score2): awale) (start: int) (last: int) : unit =
  let i = ref last in
  let taking = ref true in
  let p = index_to_player start in
  while ((!i <> precedent start) && !taking && ((index_to_player !i) <> (index_to_player start))) do
    if (board.(!i) <> 2 && board.(!i) <> 3) then (
      taking := false
    )
    else begin
      if p = Player1 then
        score1 := !score1 + board.(!i)
      else
        score2 := !score2 + board.(!i);
      board.(!i) <- 0;
      end;
    i := precedent !i;
  done; ()

(* Play a move on the awale board *)
let play_moov ((board, score1, score2): awale) (start: int) : unit =
  let i = ref (suivant start) in
  let hand = ref board.(start) in
  board.(start) <- 0;
  while !hand <> 0 do
    if !i <> start then (
      board.(!i) <- board.(!i) + 1;
      hand := !hand - 1);
    i := suivant !i;
  done;
  take (board, score1, score2) start (precedent !i);
  ()

(* Check if a move is legitimate *)
let legit_moov ((board, _, _): awale) (start: int) (current_player: player) : bool =
  let offset = if current_player = Player1 then width else 0 in
  match board.(start) with
  | 0 -> false
  | _ ->
    let board_copy = Array.copy board in
    play_moov (board_copy, ref 0, ref 0) start;
    let valid = ref false in
    for i = offset to offset + width - 1 do
      if board_copy.(i) <> 0 then valid := true
    done;
    !valid























(*
? point par case sensible adverse
-? point par case sensible
? points par graines gagnés
-? points par graines gagnés par l'adversaire.
? points par case adverse dans notre range
-? point par cases dans la range de l'adversaire.
*)
let heuristic (coef: int array) ((board, score1, score2): awale) (p: player) : int =
  let seedsMax = if p = Player1 then !score1 else !score2 in
  let seedsMin = if p = Player1 then !score2 else !score1 in
  
  let sensibleMax = ref 0 in
  let sensibleMin = ref 0 in
  
  let rangeMax = ref 0 in
  let rangeMin = ref 0 in
  
  let offsetMax = if p = Player1 then 0 else width in
  let offsetMin = if p = Player2 then 0 else width in
  
  let aRangeMax = Array.make width false in
  let aRangeMin = Array.make width false in
  
  for i = offsetMax to width + offsetMax - 1 do
    if board.(i) = 1 || board.(i) = 2 then 
      sensibleMax := !sensibleMax + 1;
    if not aRangeMax.((i - offsetMax + board.(i) + width) mod width) then begin
      rangeMax := !rangeMax + 1;
      Array.set aRangeMax ((i - offsetMax + board.(i) + width) mod width) true;
    end;
  done;
  
  for i = offsetMin to width + offsetMin - 1 do
    if board.(i) = 1 || board.(i) = 2 then 
      sensibleMin := !sensibleMin + 1 
    else if not aRangeMin.((i - offsetMin + board.(i) + width) mod width) then begin
      rangeMin := !rangeMin + 1;
      Array.set aRangeMin ((i - offsetMin + board.(i) + width) mod width) true;
    end;
  done;
  
  let final_score = coef.(0) * seedsMax + 
                    coef.(1) * seedsMin + 
                    coef.(2) * !sensibleMax + 
                    coef.(3) * !sensibleMin + 
                    coef.(4) * !rangeMax + 
                    coef.(5) * !rangeMin in
  final_score









(* Minimax algorithm to evaluate the best move *)
let rec minmax_alpha_beta ((board, score1, score2):awale) (current_player: player) (max_player: player) (depth: int) (alpha: int) (beta: int) (evaluation: awale->player->int) : int =
  if depth = 0 then
    evaluation (board, score1, score2) current_player
  else
    let alpha = ref alpha in
    let beta = ref beta in
    let next_player = other_player current_player in
    let offset = if current_player = Player1 then 0 else width in
    if current_player = max_player then (
      let max_score = ref (-1000) in
      let prune = ref false in
      for i = offset to width + offset - 1 do
        if not !prune && legit_moov (board, score1, score2) i current_player then
          let board_copy = Array.copy board in
          let score1_copy = ref !score1 in
          let score2_copy = ref !score2 in
          play_moov (board_copy, score1_copy, score2_copy) i;
          max_score := max !max_score (minmax_alpha_beta (board_copy, score1_copy, score2_copy) next_player max_player (depth - 1) !alpha !beta evaluation);
          if !max_score >= !beta then (prune := true);
          alpha := max !alpha !max_score
      done; !max_score)
    else (
      let min_score = ref (1000) in
      let prune = ref false in
      for i = offset to width + offset - 1 do
        if not !prune && legit_moov (board, score1, score2) i current_player then
          let board_copy = Array.copy board in
          let score1_copy = ref !score1 in
          let score2_copy = ref !score2 in
          play_moov (board_copy, score1_copy, score2_copy) i;
          min_score := min !min_score (minmax_alpha_beta (board_copy, score1_copy, score2_copy) next_player max_player (depth - 1) !alpha !beta evaluation);
          if !min_score <= !alpha then (prune := true);
          beta := min !beta !min_score
      done; !min_score)



(* Auxiliary function to get the best move using Minimax *)
let minmax_aux_alpha_beta ((board, score1, score2):awale) (current_player: player) (depth: int) (heuristic: awale->player->int) : int =
  let best_move = ref (-1) in
  let best_score = ref (-1000) in
  let alpha = ref (-1000) in
  let beta = ref 1000 in
  let max_player = current_player in
  let offset = if current_player = Player1 then 0 else width in
  let next_player = other_player current_player in
  for i = offset to offset + width - 1 do
    if legit_moov (board, score1, score2) i current_player then (
      let board_copy = Array.copy board in
      let score1_copy = ref !score1 in
      let score2_copy = ref !score2 in
      play_moov (board_copy, score1_copy, score2_copy) i;
      let score = minmax_alpha_beta (board_copy, score1_copy, score2_copy) next_player max_player (depth - 1) !alpha !beta heuristic in
      if score > !best_score then begin
        best_score := score;
        best_move := i;
        end;)
  done; !best_move