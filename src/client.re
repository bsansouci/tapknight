let module Node = Nodejs.Bindings_utils;

open Js.Unsafe;

open Helpers;

type posT = {x: int, y: int};

type screenCoordT =
  | ScreenCoord posT;

type gameCoordT =
  | GameCoord posT;

type dudeT = {mutable pos: gameCoordT, mutable health: int, id: string};

let getGameCoord (ScreenCoord cord) => GameCoord {x: cord.x / 200, y: cord.y / 200};

let updateDude {pos: GameCoord {x, y}} sprite => sprite#setPosition (x * 200, y * 200);

let start () => {
  let module M = {
    let pixi = Js.Unsafe.js_expr "PIXI";
  };
  /* Module alias is 10/10 better than open */
  let module R = Repixi.Repixi.Init M;
  let getTileAt (grid: list R.Sprite.t) (GameCoord cord) => List.nth grid (cord.x * 8 + cord.y);
  let createDudeSprite dude => {
    let dudeTexture = R.Texture.fromImage uri::"sprites/knight.gif";
    let sprite = (new R.Sprite.t) dudeTexture;
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
  Node.log width;
  Node.log height;
  let renderer = R.autoDetectRenderer width::width height::height;
  Js.Unsafe.set renderer#raw "backgroundColor" "0xFFFFFF";
  R.Dom.appendToBody renderer#view;
  let stage = new R.Container.t;
  let scale = 200. *. 5. /. min (width +. 60.) height;
  stage#setScale (scale, scale);
  /* let background = R.Sprite.fromImage uri::"_assets/button_test_BG.jpg";
     background#setWidth renderer#width;
     background#setHeight renderer#height;
     stage#addChild background; */
  let textureButton = R.Texture.fromImage uri::"sprites/bg.gif";
  let yourDude = {
    pos: GameCoord {x: 2, y: 2},
    id: string_of_int (Node.m (Js.Unsafe.js_expr "Date") "now" [||]),
    health: 100
  };
  let dudeSprite = createDudeSprite yourDude;
  let gridCells = ref [];
  let otherDudes: ref (list (R.Sprite.t, dudeT)) = ref [];
  let onButtonDown this => {
    let x: int = Js.Unsafe.get this#raw "x" / 200;
    let y: int = Js.Unsafe.get this#raw "y" / 200;
    /* If tile contains monster, something else happens */
    let (centerX, centerY) = (2, 2);
    let GameCoord {x: dudeX, y: dudeY} = yourDude.pos;
    let (dx, dy) = (x - centerX, y - centerY);
    if (abs dx > abs dy) {
      if (dx > 0) {
        yourDude.pos = GameCoord {x: dudeX + 1, y: dudeY}
      } else {
        yourDude.pos = GameCoord {x: dudeX - 1, y: dudeY}
      }
    } else if (
      abs dy > abs dx
    ) {
      if (dy > 0) {
        yourDude.pos = GameCoord {x: dudeX, y: dudeY + 1}
      } else {
        yourDude.pos = GameCoord {x: dudeX, y: dudeY - 1}
      }
    } else if
      /* We go along the y axis because... fuck */
      (dy > 0) {
      yourDude.pos = GameCoord {x: dudeX, y: dudeY + 1}
    } else {
      yourDude.pos = GameCoord {x: dudeX, y: dudeY - 1}
    };
    Clientsocket.emit io {typ: "move", packet: yourDude}
  };
  let everythingElseStage = new R.Container.t;
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
      fun {typ, packet} =>
        switch typ {
        | "move" =>
          print_endline "I guess trying to move";
          let {id, pos}: dudeT = packet;
          let (_, theDude) = List.find (fun (_, value) => value.id == id) !otherDudes;
          theDude.pos = pos
        | "dude-left" =>
          let dude: dudeT = packet;
          Node.log dude;
          print_endline "dude-left";
          let (otherDudeSprite, _) = List.find (fun (sprite, d) => d.id == dude.id) !otherDudes;
          Node.log otherDudeSprite;
          otherDudes := List.filter (fun (_, d) => d.id != dude.id) !otherDudes;
          ignore @@ Js.Unsafe.meth_call everythingElseStage#raw "removeChild" [|!!(otherDudeSprite#raw)|];
        | "dude-arrived" =>
          let dude: dudeT = packet;
          print_endline "dude-arrived";
          let dudeSprite = createDudeSprite dude;
          otherDudes := !otherDudes @ [(dudeSprite, dude)];
          everythingElseStage#addChild dudeSprite
        | _ => print_endline @@ "unsupported " ^ typ
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
  Clientsocket.emit io {typ: "dude-arrived", packet: yourDude}
};
