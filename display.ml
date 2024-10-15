open Main




let reset = "\x1b[0m"
let red = "\x1b[31m"
let yellow = "\x1b[33m"
let blue = "\x1b[34m"
let cyan = "\x1b[36m"

(* Print the player *)
let print_player (p:player) : unit =
  match p with
  | Player1 -> print_string "Player 1"
  | Player2 -> print_string "Player 2"

(* Print the awale game state *)
let print_awale ((board, score1, score2): awale) : unit =
  print_string (cyan ^ "\n==================================\n" ^ reset);
  print_string (blue ^ "           Awale Game             \n" ^ reset);
  print_string (cyan ^ "==================================\n" ^ reset);
  print_string (red ^ "Score Joueur 1: " ^ reset); print_int !score1; print_string "\n";
  print_string (yellow ^ "Score Joueur 2: " ^ reset); print_int !score2; print_string "\n";
  print_string (cyan ^ "----------------------------------\n" ^ reset);

  print_string (red ^ "  0   1   2   3   4   5");

  print_string (" \n");
  print_string (cyan ^ "----------------------------------\n" ^ reset);

  for j = 0 to width-1 do
    print_string (blue ^ "| " ^ reset);
    print_int board.(j);
    print_string (blue ^ " " ^ reset)
  done;
  print_string (blue ^ "|\n" ^ reset);
  print_string (cyan ^ "----------------------------------\n" ^ reset);
  for j = 1 to width do
    print_string (blue ^ "| " ^ reset);
    print_int board.((2 * width) - j);
    print_string (blue ^ " " ^ reset)
  done;
  print_string (blue ^ "|\n" ^ reset);
  print_string (cyan ^ "----------------------------------\n" ^ reset);
  
  print_string (yellow ^ " 11  10   9   8   7   6");

  print_string (" \n");
  print_string (cyan ^ "==================================\n")

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