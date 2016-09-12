let module Node = Nodejs.Bindings_utils;

open Js.Unsafe;

open Helpers;

open Common;

let getGameCoord (ScreenCoord cord) => GameCoord {x: cord.x / 200, y: cord.y / 200};

let updateDude {pos: GameCoord {x, y}} sprite => sprite#setPosition (x * 200, y * 200);

/* let handleAction (gameState: gameStateT) (action: actionT) =>
   switch action {
   | ResetState payload => payload
   | MoveDude (id, delta) =>
     switch (getDude gameState id) {
     | Some dude => moveDude gameState dude delta
     | None =>
       print_endline @@ "Hey eh... This dude doesn't exist '" ^ id ^ "'";
       gameState
     }
   | HealthChange (id, deltaHealth) =>
     switch (getDude gameState id) {
     | Some dude => changeHealth gameState dude deltaHealth
     | None =>
       print_endline @@ "Hey eh... This dude doesn't exist '" ^ id ^ "'";
       gameState
     }
   | _ => gameState
   }; */
let start () => {
  let module M = {
    let pixi = Js.Unsafe.js_expr "PIXI";
  };
  /* Module alias is 10/10 better than open */
  let module R = Repixi.Repixi.Init M;
  let getTileAt (grid: list R.Sprite.t) (GameCoord cord) => List.nth grid (cord.x * 8 + cord.y);
  let createDudeSprite dude => {
    let dudeTexture =
      if dude.friendly {
        R.Texture.fromImage uri::"sprites/knight.gif"
      } else {
        R.Texture.fromImage uri::"sprites/dino.gif"
      };
    let sprite = (new R.Sprite.t) dudeTexture;
    Js.Unsafe.set sprite#raw "tint" dude.tint;
    Node.log (Js.Unsafe.get sprite#raw "tint");
    sprite#setButtonMode true;
    let GameCoord {x, y} = dude.pos;
    sprite#setPosition (x * 200, y * 200);
    sprite#setInteractive true;
    sprite
    /* tile#on MouseDown onButtonDown;
       tile#on TouchStart onButtonDown;
       tile#on MouseUp onButtonUp;
       tile#on TouchEnd onButtonUp;
       tile#on MouseUpOutside onButtonUp;
       tile#on TouchEndOutside onButtonUp; */
  };
  let io = Clientsocket.start ();
  let width = Js.Unsafe.get (Js.Unsafe.js_expr "window") "innerWidth";
  let height = Js.Unsafe.get (Js.Unsafe.js_expr "window") "innerHeight";
  let minSize = min (width +. 60.) height;
  let renderer = R.autoDetectRenderer width::(int_of_float minSize) height::(int_of_float minSize);
  Js.Unsafe.set renderer#raw "backgroundColor" "0xFFFFFF";
  R.Dom.appendToBody renderer#view;
  let stage = new R.Container.t;
  let scale = 200. *. 5. /. minSize;
  stage#setScale (scale, scale);
  let textureButton = R.Texture.fromImage uri::"sprites/bg.gif";
  let yourDude = {
    pos: GameCoord {x: 2, y: 2},
    id: string_of_int (Node.m (Js.Unsafe.js_expr "Date") "now" [||]),
    health: 100,
    tint: Js.Unsafe.js_expr "Math.random() * 0xFFFFFF",
    friendly: true
  };
  let dudeSprite = createDudeSprite yourDude;
  let gridCells = ref [];
  let otherDudes: ref (list (R.Sprite.t, dudeT)) = ref [];
  let gameState = {otherDudes: []};
  let onButtonDown this => {
    let x: int = Js.Unsafe.get this#raw "x" / 200;
    let y: int = Js.Unsafe.get this#raw "y" / 200;
    /* If tile contains monster, something else happens */
    let (centerX, centerY) = (2, 2);
    let (dx, dy) = (x - centerX, y - centerY);
    let delta =
      if (abs dx > abs dy) {
        dx > 0 ? {x: 1, y: 0} : {x: (-1), y: 0}
      } else {
        dy > 0 ? {x: 0, y: 1} : {x: 0, y: (-1)}
      };
    let GameCoord {x: dudeX, y: dudeY} = yourDude.pos;
    yourDude.pos = GameCoord {x: dudeX + delta.x, y: dudeY + delta.y};
    Clientsocket.emit io (MoveDude (yourDude.id, GameCoord delta))
  };
  let everythingElseStage = new R.Container.t;
  let removeDude dude => {
    let (otherDudeSprite, _) = List.find (fun (sprite, d) => d.id == dude.id) !otherDudes;
    otherDudes := List.filter (fun (_, d) => d.id != dude.id) !otherDudes;
    ignore @@ Js.Unsafe.meth_call everythingElseStage#raw "removeChild" [|!!otherDudeSprite#raw|]
  };
  let onButtonDown2 this => {
    let (_, clickedDude) = List.find (fun (sprite, dude) => sprite == this) !otherDudes;
    let deltaHealth = (-50);
    Clientsocket.emit io (HealthChange (clickedDude.id, deltaHealth))
  };
  R.Events.(
    for i in 0 to 4 {
      for j in 0 to 4 {
        let tile = (new R.Sprite.t) textureButton;
        tile#setButtonMode true;
        tile#setPosition (i * 200, j * 200);
        tile#setInteractive true;
        tile#on MouseDown onButtonDown;
        tile#on TouchStart onButtonDown;
        stage#addChild tile;
        gridCells := !gridCells @ [tile]
      }
    }
  );
  everythingElseStage#addChild dudeSprite;
  stage#addChildContainer everythingElseStage;
  Clientsocket.on
    io
    (
      fun message =>
        /* actionQueue := [message, ...!actionQueue]; */
        switch message {
        | MoveDude (id, GameCoord {x: deltaX, y: deltaY}) =>
          print_endline @@ "I guess trying to move " ^ id;
          let (_, {pos: GameCoord {x: dudeX, y: dudeY}} as theDude) =
            List.find (fun (_, d) => d.id == id) !otherDudes;
          theDude.pos = GameCoord {x: dudeX + deltaX, y: dudeY + deltaY}
        /* switch (getDude gameState id) {
           | Some theDude => theDude.pos = pos
           | None => print_endline "wat"
           } */
        | RemoveDude id =>
          print_endline "dude-left";
          let (_, theDude) = List.find (fun (_, d) => d.id == id) !otherDudes;
          removeDude theDude
        /* switch (getDude gameState id) {
           | Some theDude => removeDude theDude
           | None => print_endline "wat"
           } */
        | AddDude dude =>
          print_endline @@ "dude-arrived " ^ dude.id;
          let dudeSprite = createDudeSprite dude;
          otherDudes := !otherDudes @ [(dudeSprite, dude)];
          dudeSprite#on MouseDown onButtonDown2;
          dudeSprite#on TouchStart onButtonDown2;
          everythingElseStage#addChild dudeSprite
        | _ => print_endline @@ "unsupported type"
        }
    );
  let rec animate () => {
    Dom_html._requestAnimationFrame (Js.wrap_callback animate);
    updateDude yourDude dudeSprite;
    let (dudeX, dudeY) = dudeSprite#position;
    Js.Unsafe.set everythingElseStage#raw "x" (- dudeX + 400);
    Js.Unsafe.set everythingElseStage#raw "y" (- dudeY + 400);
    ignore @@ List.map (fun (sprite, dude) => updateDude dude sprite) !otherDudes;
    renderer#render stage
  };
  animate ();
  Clientsocket.emit io (AddDude yourDude)
};
