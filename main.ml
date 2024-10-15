let width = 6
let height = 2
let nb_seeds = 4

(* The awale game state includes the board and the scores of both players *)
type awale = int array * int ref * int ref

(* Define the two players *)
type player = 
  | Player1
  | Player2

(* Initialize the awale game *)
let init_awale () : awale =
  let board = Array.make (width * height) nb_seeds in
  let score1 = ref 0 in
  let score2 = ref 0 in
  (board, score1, score2)

(* Convert an index to a player *)
let index_to_player (index: int) : player =
  if index mod (width * height) < width then Player1 else Player2

(* Get the other player *)
let other_player (p: player) : player =
  match p with
  | Player1 -> Player2
  | Player2 -> Player1

(* Print the player *)
let print_player player =
  match player with
  | Player1 -> print_string "Joueur 1"
  | Player2 -> print_string "Joueur 2"

(* Print the awale game state *)
let print_awale ((board, score1, score2): awale) : unit =
  (* Codes de couleur ANSI *)
  let reset = "\x1b[0m" in
  let red = "\x1b[31m" in
  let yellow = "\x1b[33m" in
  let blue = "\x1b[34m" in
  let cyan = "\x1b[36m" in

  (* Affichage du plateau *)
  print_string (cyan ^ "\n==================================\n" ^ reset);
  print_string (blue ^ "           Awale Game             \n" ^ reset);
  print_string (cyan ^ "==================================\n" ^ reset);
  print_string (red ^ "Score Joueur 1: " ^ reset); print_int !score1; print_string "\n";
  print_string (yellow ^ "Score Joueur 2: " ^ reset); print_int !score2; print_string "\n";
  print_string (cyan ^ "----------------------------------\n" ^ reset);
  for i = 0 to width-1 do
    print_string ("  ");
    print_int i;
    print_string (" ")
  done;
  print_string (" \n");
  print_string (cyan ^ "----------------------------------\n" ^ reset);
  for i = 0 to height-1 do
    for j = 0 to width-1 do
      print_string (blue ^ "| " ^ reset);
      print_int board.(i * width + j);
      print_string (blue ^ " " ^ reset)
    done;
    print_string (blue ^ "|\n" ^ reset);
    print_string (cyan ^ "----------------------------------\n" ^ reset);
  print_string (cyan ^ "----------------------------------\n" ^ reset);
  done;
  for i = width to 2*width-1 do
    print_string ("  ");
    print_int i;
    print_string (" ")
  done;
  print_string (" \n");
  print_string (cyan ^ "==================================\n")


(* Get the next position in the awale board *)
let suivant (index: int) : int =
  if index = 11 then 5 
  else if index = 0 then 6 
  else if index > 5 then index + 1 
  else if index < 6 then index - 1 
  else -1

(* Get the previous position in the awale board *)
let precedent (index: int) : int =
  if index = 5 then 11 
  else if index = 6 then 0 
  else if index > 6 then index - 1 
  else if index < 5 then index + 1 
  else -1

(* Donne le numéro de la case actuelle après s'être déplacé de n graines *)
let rec avancer (board: int array) (start: int) (graine: int) : int =
  if graine = 0 then start
  else avancer board (suivant start) (graine - 1)

(* Check if a move is legitimate *)
let legit_moov ((board, _, _): awale) (start: int) (current_player: player) : bool =
  let offset = 
    match current_player with
    | Player1 -> 0
    | Player2 -> 6
  in
    match board.(start) with
    | 0 -> false
    | _ ->
      let survivant = ref false in
      let voisin_mange = ref false in
      for i = (if offset = 0 then 0 else 11) to (if offset = 0 then 5 else 6) do
        if (((start = offset + i) || !voisin_mange) && (board.(offset + i) = 1 || board.(offset + i) = 2))
          then voisin_mange := true
        else (voisin_mange := false ; if board.(offset + i) <> 0 then survivant := true)
      done;
      ((offset <= start && start < (offset + 6)) && (((avancer board start board.(start)) - offset < 6) || !survivant))

(* Take seeds from the board *)
let take ((board, score1, score2): awale) (start: int) (last: int) : unit =
  let i = ref last in
  let taking = ref true in
  let p = index_to_player start in
  while ((!i <> (precedent start)) && !taking && ((index_to_player !i) <> (index_to_player start))) do
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
    if !i <> start then
      board.(!i) <- board.(!i) + 1;
      hand := !hand - 1;
    i := suivant !i;
  done;
  take (board, score1, score2) start (precedent !i);
  ()


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

(*
-naif coef: {1, -1, 0, 0, 0, 0}
-smart coef:{2, -2, 1, -1, 0, 0}
-gigasmart coef:{5, -5, 3, -3, 2, -2}
-offensive coef:{5, -4, 3, -2, 2, -1}
-deffensive coef:{4, -5, 2, -3, 1, -2}
*)

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



let play1vs1 () : unit =
  let board, score1, score2 = init_awale () in
  let current_player = ref Player1 in
  while true do
    print_awale (board, score1, score2);
    print_string "\n";
    print_player !current_player;
    print_string "'s turn. Enter your move: ";
    let move = read_int () in
    print_string "\n";
  
    if legit_moov (board, score1, score2) move !current_player then begin
      play_moov (board, score1, score2) move;
      current_player := other_player !current_player
    end else
      print_string "Invalid move. Try again.\n";
  done

let play1vsBot (player_start:bool) (depth:int) (heuristic: awale->player->int) : unit =
  let board, score1, score2 = init_awale () in
  let current_player = if player_start then ref Player1 else ref Player2 in
  while true do
    print_awale (board, score1, score2);
    print_string "\n";
    print_string "heuristic score: "; print_int (heuristic (board, score1, score2) Player1);
    print_string "\n";
    print_player !current_player;
    print_string "'s turn. Enter your move: ";

    let move = ref 0 in
    if !current_player = Player1 then (
      move := read_int ()
    )
    else (
      move := minmax_aux_alpha_beta (board, score1, score2) Player2 depth heuristic;
      print_int !move);
    
    print_string "\n";
    if legit_moov (board, score1, score2) !move !current_player then begin
      play_moov (board, score1, score2) !move;
      current_player := other_player !current_player
    end else
      print_string "Invalid move. Try again.\n";
  done

let playBotVsBot (depth: int) (heuristic1: awale -> player -> int) (heuristic2: awale -> player -> int) : unit =
  let board, score1, score2 = init_awale () in
  let current_player = ref Player1 in
  while true do
    print_awale (board, score1, score2);
    print_string "\n";
    print_string "Heuristic score: "; 
    if !current_player = Player1 then
      print_int (heuristic1 (board, score1, score2) !current_player)
    else
      print_int (heuristic2 (board, score1, score2) !current_player);
    print_string "\n";
    
    print_player !current_player; 
    print_string "'s turn.\n";
    
    let move = ref (-1) in
    if !current_player = Player1 then
      move := minmax_aux_alpha_beta (board, score1, score2) !current_player depth heuristic1
    else
      move := minmax_aux_alpha_beta (board, score1, score2) !current_player depth heuristic2;
    
    print_int !move; 
    print_string "\n";
    
    if legit_moov (board, score1, score2) !move !current_player then begin
      play_moov (board, score1, score2) !move;
      current_player := other_player !current_player
    end else
      print_string "Invalid move. Something went wrong.\n";
  done



(*coef = [|nbSeedsMax; nbSeedsMin; sensibleMax; sensibleMin; rangeMax; rangeMin|]*)
let test_coef = [|0; 0; 0; 0; 0; 0|]
let naif_coef = [|1; -1; 0; 0; 0; 0|]
let smart_coef = [|5; -5; -1; 1; 0; 0|]
let gigasmart_coef = [|10; -10; -2; 2; 2; -2|]

(*let () = play1vs1 ()*)
let () = play1vsBot true 12 (heuristic gigasmart_coef)
(*let () = playBotVsBot 8 (heuristic smart_coef) (heuristic naif_coef)*)