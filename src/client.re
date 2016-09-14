let module Node = Nodejs.Bindings_utils;

open Js.Unsafe;

open Helpers;

open Common;

let moveSpeed = 80.;

let getGameCoord (ScreenCoord cord) => GameCoord {x: cord.x / 200, y: cord.y / 200};

let updateDude {pos: GameCoord {x, y}} sprite => sprite#setPosition (x * 200, y * 200);

let get key map =>
  try (Some (DudeMap.find key map)) {
  | Not_found => None
  };

type isAnimationDoneT =
  | Done gameStateT
  | NotDone gameStateT;

type animationT = {
  elapsedTime: float,
  dudeID: idT,
  callback: gameStateT => float => isAnimationDoneT
};

let start () => {
  let module M = {
    let pixi = Js.Unsafe.js_expr "PIXI";
  };
  /* Module alias is 10/10 better than open */
  let module R = Repixi.Repixi.Init M;
  /* let getTileAt (grid: list R.Sprite.t) (GameCoord cord) => List.nth grid (cord.x * 8 + cord.y); */
  let createDudeSprite dude onOtherDudeTap => {
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
    sprite#on MouseDown (onOtherDudeTap dude);
    sprite#on TouchStart (onOtherDudeTap dude);
    sprite
  };
  let onLoad () => {
    let io = Clientsocket.start ();
    let yourID = Js.to_string (Js.Unsafe.js_expr {|Date.now().toString()|});
    let width = Js.Unsafe.get (Js.Unsafe.js_expr "window") "innerWidth";
    let height = Js.Unsafe.get (Js.Unsafe.js_expr "window") "innerHeight";
    let minSize = min (width -. 30.) height;
    let renderer =
      R.autoDetectRenderer width::(int_of_float minSize) height::(int_of_float minSize);
    Js.Unsafe.set renderer#raw "backgroundColor" "0xFFFFFF";
    R.Dom.appendToBody renderer#view;
    let stage = new R.Container.t;
    let scale = minSize /. (200. *. 5.);
    stage#setScale (scale, scale);
    let dude2Sprites: ref (DudeMap.t R.Sprite.t) = ref DudeMap.empty;
    let gameState = ref {dudes: []};
    let actionQueue: ref (list actionT) = ref [];
    let animationList: ref (list animationT) = ref [];
    let everythingElseStage = new R.Container.t;
    let onOtherDudeTap clickedDude this =>
      if (not clickedDude.friendly) {
        let deltaHealth = (-51);
        let action = HealthChange (clickedDude.id, deltaHealth);
        Clientsocket.emit io action;
        actionQueue := [action, ...!actionQueue]
      } else {
        print_endline @@ "probably healing?"
      };
    let resetSprites newGameState dude2Sprites => {
      /* Reimplementation of MeltGoldIntoMold */
      let dudesToRemove = ref [];

      /** Aggregate all the dudes to be removed, then remove them from the map and remove their sprites **/
      DudeMap.iter
        (
          fun currentDude currentSprite => {
            let keepCurrentDude = List.mem currentDude newGameState.dudes;
            if (not keepCurrentDude) {
              dudesToRemove := [(currentDude, currentSprite), ...!dudesToRemove]
            }
          }
        )
        !dude2Sprites;
      print_endline @@ "dudes to remove " ^ string_of_int (List.length !dudesToRemove);
      List.iter
        (
          fun (dudeToRemove, spriteToRemove) => {
            /* SIDE EFFECT */
            ignore @@
            Js.Unsafe.meth_call everythingElseStage#raw "removeChild" [|!!spriteToRemove#raw|];
            dude2Sprites := DudeMap.remove dudeToRemove !dude2Sprites
          }
        )
        !dudesToRemove;

      /** Aggregate all the dudes to be added **/
      List.iter
        (
          fun currentDude => {
            let maybeSprite = get currentDude !dude2Sprites;
            print_endline @@ "Dude: " ^ dudeToString currentDude;
            switch maybeSprite {
            | Some sprite => /* Don't do anything */ ()
            | None =>
              /* SIDE EFFECT */
              let dudeSprite = createDudeSprite currentDude onOtherDudeTap;
              dude2Sprites := DudeMap.add currentDude dudeSprite !dude2Sprites;
              everythingElseStage#addChild dudeSprite
            }
          }
        )
        newGameState.dudes
    };
    let animateMoveDude
        dude::dude
        delta::(GameCoord delta)
        totalTime::totalTime
        gameState
        elapsedTime => {
      let GameCoord startValue = dude.pos;
      switch (get dude !dude2Sprites) {
      | Some sprite =>
        if (elapsedTime >= totalTime) {
          sprite#setPositionf (
            (float_of_int startValue.x +. float_of_int delta.x) *. 200.,
            (float_of_int startValue.y +. float_of_int delta.y) *. 200.
          );
          Done gameState
        } else {
          sprite#setPositionf (
            (float_of_int startValue.x +. float_of_int delta.x *. (elapsedTime /. totalTime)) *. 200.,
            (float_of_int startValue.y +. float_of_int delta.y *. (elapsedTime /. totalTime)) *. 200.
          );
          NotDone gameState
        }
      | None => Done gameState
      }
    };
    let handleAction (action: actionT) (gameState: gameStateT) =>
      switch action {
      | ResetState newGameState =>
        print_endline "reset gamestate";
        resetSprites newGameState dude2Sprites;
        newGameState
      | RemoveDude id =>
        print_endline "dude-left";
        switch (getDude gameState id) {
        | Some dude =>
          switch (get dude !dude2Sprites) {
          | Some sprite =>
            /* SIDE EFFECT */
            ignore @@ Js.Unsafe.meth_call everythingElseStage#raw "removeChild" [|!!sprite#raw|];
            removeDude gameState dude
          | None => gameState
          }
        | None => assert false
        }
      | AddDude dude =>
        print_endline @@ "dude-arrived " ^ dudeToString dude;
        let dudeSprite = createDudeSprite dude onOtherDudeTap;
        dude2Sprites := DudeMap.add dude dudeSprite !dude2Sprites;
        everythingElseStage#addChild dudeSprite;
        addDude gameState dude
      | MoveDude (id, delta) =>
        switch (getDude gameState id) {
        | Some dude =>
          switch (moveDude gameState dude delta) {
          | Some nextGameState =>
            /* SIDE EFFECT */
            animationList := List.filter (fun {dudeID} => dudeID != dude.id) !animationList;
            let callback = animateMoveDude dude::dude delta::delta totalTime::moveSpeed;
            animationList := [{dudeID: dude.id, elapsedTime: 0., callback}, ...!animationList];
            nextGameState
          | None => gameState
          }
        | None =>
          print_endline @@ "Hey eh... This dude doesn't exist '" ^ id ^ "'";
          gameState
        }
      | HealthChange (id, deltaHealth) =>
        switch (getDude gameState id) {
        | Some dude =>
          let newGameState = changeHealth gameState dude deltaHealth;
          resetSprites newGameState dude2Sprites;
          newGameState
        | None =>
          print_endline @@ "Hey eh... This dude doesn't exist '" ^ id ^ "'";
          gameState
        }
      };
    let onButtonDown gameState this =>
      switch (getDude !gameState yourID) {
      | Some yourDude =>
        let maybeAnimation = find (fun {dudeID} => dudeID == yourDude.id) !animationList;
        switch maybeAnimation {
        | Some animation => ()
        | None =>
          let x: int = Js.Unsafe.get this#raw "x" / 200;
          let y: int = Js.Unsafe.get this#raw "y" / 200;
          /* If tile contains monster, something else happens */
          let GameCoord {x: centerX, y: centerY} = yourDude.pos;
          let (dx, dy) = (x - centerX, y - centerY);
          let delta =
            if (abs dx > abs dy) {
              dx > 0 ? {x: 1, y: 0} : {x: (-1), y: 0}
            } else {
              dy > 0 ? {x: 0, y: 1} : {x: 0, y: (-1)}
            };
          let action = MoveDude (yourDude.id, GameCoord delta);
          Clientsocket.emit io action;
          actionQueue := [action, ...!actionQueue]
        }
      | None =>
        print_endline "Your dude got killed";
        assert false
      };
    let textureButton = R.Texture.fromImage uri::"sprites/bg.gif";
    R.Events.(
      for i in 0 to 40 {
        for j in 0 to 40 {
          let tile = (new R.Sprite.t) textureButton;
          tile#setButtonMode true;
          tile#setPosition (i * 200, j * 200);
          tile#setInteractive true;
          tile#on MouseDown (onButtonDown gameState);
          tile#on TouchStart (onButtonDown gameState);
          everythingElseStage#addChild tile
        }
      }
    );
    stage#addChildContainer everythingElseStage;
    Clientsocket.on io (fun message => actionQueue := [message, ...!actionQueue]);
    let prevTime = ref 0.;
    let rec animate time => {
      let deltaTime = time -. !prevTime;
      prevTime := time;
      R.requestAnimationFrame animate;
      gameState := List.fold_right handleAction !actionQueue !gameState;
      /* Empty action queue after handling the events */
      actionQueue := [];

      /** Handle animations here. Updates the gamestate and the animationList **/
      let (newGameState, newAnimationlist) =
        !animationList |>
        List.map (fun animation => {...animation, elapsedTime: animation.elapsedTime +. deltaTime}) |>
        List.fold_left
          (
            fun (gameState, animationsToKeep) ({callback, elapsedTime} as currentAnimation) =>
              switch (callback gameState elapsedTime) {
              | NotDone newGameState => (newGameState, [currentAnimation, ...animationsToKeep])
              | Done newGameState => (newGameState, animationsToKeep)
              }
          )
          (!gameState, []);
      gameState := newGameState;
      animationList := newAnimationlist;
      switch (getDude !gameState yourID) {
      | Some yourDude =>
        /** Update the whole stage to keep yourDude centered **/
        switch (get yourDude !dude2Sprites) {
        | Some dudeSprite =>
          let (dudeX, dudeY) = dudeSprite#position;
          Js.Unsafe.set everythingElseStage#raw "x" (- dudeX + 400);
          Js.Unsafe.set everythingElseStage#raw "y" (- dudeY + 400)
        | None => assert false
        }
      | None => /* You don't have your own dude yet... I guess... */ ()
      };
      renderer#render stage
    };
    animate 0.;
    Node.m io "emit" [|putStr "join", !!(Js.string yourID)|]
  };
  Js.Unsafe.set (Js.Unsafe.js_expr "window") "onload" !@onLoad
};
