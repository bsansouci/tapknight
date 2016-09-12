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
  let scale = 200. *. 5. /. (min (width +. 60.) height);
  stage#setScale (scale, scale);
  /* let background = R.Sprite.fromImage uri::"_assets/button_test_BG.jpg";
     background#setWidth renderer#width;
     background#setHeight renderer#height;
     stage#addChild background; */
  let textureButton = R.Texture.fromImage uri::"sprites/bg.gif";
  let dude = {pos: GameCoord {x: 2, y: 2}, id: "kek-lord", health: 100};
  let dudeSprite = createDudeSprite dude;
  let gridCells = ref [];
  let otherDudes: ref (list dudeT) = ref [];
  let otherDudesSprites: ref (list R.Sprite.t) = ref [];
  let onButtonDown this => {
    let x: int = Js.Unsafe.get this#raw "x" / 200;
    let y: int = Js.Unsafe.get this#raw "y" / 200;
    /* If tile contains monster, something else happens */
    let (centerX, centerY) = (2, 2);
    let GameCoord {x: dudeX, y: dudeY} = dude.pos;
    let (dx, dy) = (x - centerX, y - centerY);
    if (abs dx > abs dy) {
      if (dx > 0) {
        dude.pos = GameCoord {x: dudeX + 1, y: dudeY}
      } else {
        dude.pos = GameCoord {x: dudeX - 1, y: dudeY}
      }
    } else if (
      abs dy > abs dx
    ) {
      if (dy > 0) {
        dude.pos = GameCoord {x: dudeX, y: dudeY + 1}
      } else {
        dude.pos = GameCoord {x: dudeX, y: dudeY - 1}
      }
    } else if
      /* We go along the y axis because... fuck */
      (dy > 0) {
      dude.pos = GameCoord {x: dudeX, y: dudeY + 1}
    } else {
      dude.pos = GameCoord {x: dudeX, y: dudeY - 1}
    };
    Clientsocket.emit io {typ: "move", packet: dude}
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
          Node.log (List.nth !otherDudes 0).id;
          let theDude = List.find (fun value => value.id == id) !otherDudes;
          theDude.pos = pos;
        | "new-dude-arrived" =>
          let dude: dudeT = packet;
          print_endline "new-dude-arrived";
          otherDudes := !otherDudes @ [dude];
          let dudeSprite = createDudeSprite dude;
          otherDudesSprites := !otherDudesSprites @ [dudeSprite];
          everythingElseStage#addChild dudeSprite
        | _ => print_endline @@ "unsupported " ^ typ
        }
    );
  let rec animate () => {
    Dom_html._requestAnimationFrame (Js.wrap_callback animate);
    updateDude dude dudeSprite;
    let (dudeX, dudeY) = dudeSprite#position;
    Js.Unsafe.set everythingElseStage#raw "x" (-dudeX + 400);
    Js.Unsafe.set everythingElseStage#raw "y" (-dudeY + 400);
    ignore @@ List.map2 (fun dude sprite => updateDude dude sprite) !otherDudes !otherDudesSprites;
    renderer#render stage
  };
  animate ();
  Clientsocket.emit io {typ: "new-dude-arrived", packet: dude}
};
