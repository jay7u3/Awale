let width = 6
let height = 2
let initHashtableLength = 100

let nbSquare = width*height
let nbSeedsMax = 48

let zobristPlayer1 = Array.init nbSquare (fun _ -> Array.init nbSeedsMax (fun _ -> Random.bits ()))
let zobristPlayer2 = Array.init nbSquare (fun _ -> Array.init nbSeedsMax (fun _ -> Random.bits ()))

let moovSaved : (int, int) Hashtbl.t = Hashtbl.create initHashtableLength

type player = 
  | Player1
  | Player2

let zobrist_hash (board: int array) (current_player: player) : int =
  let hash = ref 0 in
  for i = 0 to (Array.length board) - 1 do
    let seed_count = board.(i) in
    if current_player = Player1 then
      hash := !hash lxor zobristPlayer1.(i).(seed_count)
    else
      hash := !hash lxor zobristPlayer2.(i).(seed_count)
  done;
  !hash

let add_move (hash_table:(int, int) Hashtbl.t) (key:int) (move:int) : unit =
  Hashtbl.add hash_table key move

let get_move hash_table key = Hashtbl.find hash_table key
