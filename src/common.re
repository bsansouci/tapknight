type vecT = {x: int, y: int};

type screenCoordT =
  | ScreenCoord vecT;

type gameCoordT =
  | GameCoord vecT;

type tintT = int;

type idT = string;

type dudeT = {mutable pos: gameCoordT, mutable health: int, id: idT, tint: tintT, friendly: bool};

type gameStateT = {otherDudes: list dudeT};

type actionT =
  | ResetState gameStateT
  | AddDude dudeT
  | RemoveDude idT
  | ChangeHealth
  | MoveDude (idT, gameCoordT)
  | HealthChange (idT, int);

let getDude gameState id =>
  switch (List.filter (fun dude => dude.id == id) gameState.otherDudes) {
  | [dude] => Some dude
  | _ => None
  };

let moveDude gameState dude (GameCoord delta) => {
  let GameCoord {x: dudeX, y: dudeY} = dude.pos;
  let newPos = GameCoord {x: dudeX + delta.x, y: dudeY + delta.y};
  {otherDudes: List.map (fun d => dude == d ? {...dude, pos: newPos} : d) gameState.otherDudes}
};

let changeHealth gameState dude deltaHealth => {
  otherDudes:
    gameState.otherDudes |>
    List.map (fun d => d.id == dude.id ? {...d, health: d.health + deltaHealth} : d) |>
    List.filter (fun d => d.health > 0)
};
