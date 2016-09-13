type vecT = {x: int, y: int};

type screenCoordT =
  | ScreenCoord vecT;

type gameCoordT =
  | GameCoord vecT;

type tintT = int;

type idT = string;

type dudeT = {pos: gameCoordT, health: int, id: idT, tint: tintT, friendly: bool};

type gameStateT = {dudes: list dudeT};

type actionT =
  | ResetState gameStateT
  | AddDude dudeT
  | RemoveDude idT
  | MoveDude (idT, gameCoordT)
  | HealthChange (idT, int);

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
    gameState
  } else {
    {dudes: List.map (fun d => dude == d ? {...dude, pos: GameCoord newPos} : d) gameState.dudes}
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
  | ResetState gameState => "ResetState" ^ gameStateToString gameState
  | AddDude dude => "AddDude " ^ dudeToString dude
  | RemoveDude id => "Remove " ^ id
  | MoveDude (id, gameCoord) => "ModeDude " ^ id ^ " at " ^ gameCoordToString gameCoord
  | HealthChange (id, delta) => "HealthChange " ^ id ^ " of " ^ string_of_int delta
  };

let module DudeComparator = {
  type t = dudeT;
  let compare a b => {
    let id1 = a.id;
    let id2 = b.id;
    String.compare id1 id2
  };
};

let module DudeMap = Map.Make DudeComparator;
