type vecT = {x: int, y: int};

type screenCoordT =
  | ScreenCoord vecT;

type gameCoordT =
  | GameCoord vecT;

type tintT = int;

type idT = string;

type dudeT = {pos: gameCoordT, health: int, id: idT, tint: tintT, friendly: bool};

type gameStateT = {dudes: list dudeT};

/* TODO: This is garbage. ResocketIO isn't made to handle variants with data associated with it.
  * Plz someone refactor this. We should be doing
  * Server.on socket (fun packet =>
  *  switch (packet) =>
  *  | ResetState gameState => ...
  *  ...
  * );
 */
module Action = {
  type dataT =
    | ResetState gameStateT
    | AddDude dudeT
    | RemoveDude idT
    | MoveDude (idT, gameCoordT)
    | HealthChange (idT, int);
  type t =
    | Action
    | Join
    | Disconnect;
  /* type t 'a =
     | Action :t 'a
     | Join :t string
     | Disconnect :t unit; */
  /* let stringify a => a; */
  /* let reverse (type a) s : t a =>
     switch s {
     | "action" => Action
     | "join" => Join
     | "disconnect" => Disconnect
     }; */
  let stringify: t => string =
    fun
    | Action => "action"
    | Join => "join"
    | Disconnect => "disconnect";
  /* let stringify: type a. t a => string =
     fun
     | Action => "action"
     | Join => "join"
     | Disconnect => "disconnect"; */
  let all: list t = [Action, Join, Disconnect];
  /* type term _ =
       | Pair (term 'a) (term 'b) :term ('a, 'b)
       | Fst (term ('a, 'b)) :term 'a
       | Snd (term ('a, 'b)) :term 'b;
     let eval: type a. term a => a =
       fun
       | Pair x y [@implicit_arity] => (x, y)
       | Fst x _ [@implicit_arity] => x
       | Snd _ y [@implicit_arity] => y; */
};

let find cb l =>
  switch (List.filter cb l) {
  | [] => None
  | [x, ...rest] => Some x
  };

let getDude gameState id =>
  switch (List.filter (fun dude => dude.id == id) gameState.dudes) {
  | [dude] => Some dude
  | _ => None
  };

let moveDude gameState dude (GameCoord delta) => {
  let GameCoord {x: dudeX, y: dudeY} = dude.pos;
  let newPos = {x: dudeX + delta.x, y: dudeY + delta.y};
  let collides = List.exists (fun {pos: GameCoord vec} => vec == newPos) gameState.dudes;
  if collides {
    None
  } else {
    Some {
      dudes: List.map (fun d => dude == d ? {...dude, pos: GameCoord newPos} : d) gameState.dudes
    }
  }
};

let changeHealth gameState dude deltaHealth => {
  dudes:
    gameState.dudes |>
    List.map (fun d => d.id == dude.id ? {...d, health: d.health + deltaHealth} : d) |>
    List.filter (fun d => d.health > 0)
};

let removeDude gameState dude => {
  dudes: gameState.dudes |> List.filter (fun d => d.id != dude.id)
};

let addDude gameState dude => {dudes: [dude, ...gameState.dudes]};

let vecToString {x, y} => "{x: " ^ string_of_int x ^ ", y: " ^ string_of_int y ^ "}";

let dudeToString {pos: GameCoord vec, health, id, friendly} =>
  "{id: " ^
  id ^
  ", pos: " ^
  vecToString vec ^
  ", health: " ^ string_of_int health ^ ", friendly: " ^ string_of_bool friendly ^ "}";

let gameStateToString gameState =>
  "{dudes: [" ^
  (
    switch gameState.dudes {
    | [] => ""
    | [dude, ...rest] =>
      dudeToString dude ^ List.fold_left (fun acc dude => ", " ^ acc ^ dudeToString dude) "" rest
    }
  ) ^ "]}";

let gameCoordToString (GameCoord vec) => "GameCoord " ^ vecToString vec;

let actionToString action =>
  switch action {
  | Action.ResetState gameState => "ResetState" ^ gameStateToString gameState
  | Action.AddDude dude => "AddDude " ^ dudeToString dude
  | Action.RemoveDude id => "Remove " ^ id
  | Action.MoveDude (id, gameCoord) => "MoveDude " ^ id ^ " at " ^ gameCoordToString gameCoord
  | Action.HealthChange (id, delta) => "HealthChange " ^ id ^ " of " ^ string_of_int delta
  };

module DudeComparator = {
  type t = dudeT;
  let compare a b => String.compare a.id b.id;
};

module DudeMap = Map.Make DudeComparator;

let makeID () => {
  let s4 () => Printf.sprintf "%04x" (Random.int 65536);
  s4 () ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ s4 () ^ s4 ()
};
