let module Node = Nodejs.Bindings_utils;

open Js.Unsafe;

open Helpers;

open Common;

let moveSpeed = 80.;

let getGameCoord (ScreenCoord cord) => GameCoord {x: cord.x / 200, y: cord.y / 200};

let updateDude {pos: GameCoord {x, y}} sprite => sprite#setPosition (x * 200 + 100, y * 200 + 100);

let clamp m v => v > m ? m : v;

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

let defaultDamage = (-10);

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
    sprite#setButtonMode true;
    let GameCoord {x, y} = dude.pos;
    sprite#setPosition (x * 200 + 100, y * 200 + 100);
    sprite#setInteractive true;
    sprite#on MouseDown (onOtherDudeTap dude);
    sprite#on TouchStart (onOtherDudeTap dude);
    sprite#setAnchor (0.5, 0.5);
    sprite
  };
  let onLoad () => {
    let io = Clientsocket.start ();
    let yourID = Js.to_string (Js.Unsafe.js_expr {|Date.now().toString()|});
    let width = Js.Unsafe.get (Js.Unsafe.js_expr "window") "innerWidth";
    let height = Js.Unsafe.get (Js.Unsafe.js_expr "window") "innerHeight";
    let minSize = min (width -. 30.) height;
    let mainGameView =
      R.autoDetectRenderer width::(int_of_float minSize) height::(int_of_float minSize);
    Js.Unsafe.set mainGameView#raw "backgroundColor" "0xFFFFFF";
    R.Dom.appendToBody mainGameView#view;
    let controlsView =
      R.autoDetectRenderer
        width::(int_of_float minSize) height::(int_of_float (height -. minSize));
    Js.Unsafe.set controlsView#raw "backgroundColor" "0xFFFFFF";
    R.Dom.appendToBody controlsView#view;

    /** Create different top level containers **/
    let stage = new R.Container.t;
    let controlsStage = new R.Container.t;
    let scale = minSize /. (200. *. 5.);
    stage#setScale (scale, scale);
    controlsStage#setScale (scale, scale);
    print_endline @@ "->" ^ string_of_float (minSize /. 2.);
    Js.Unsafe.set
      stage#raw
      "pivot"
      !!(
        Js.Unsafe.new_obj (Js.Unsafe.js_expr "PIXI.Point") [|!!(minSize /. 2.), !!(minSize /. 2.)|]
      );
    stage#setPositionf (minSize /. 2., minSize /. 2.);
    let damageYouDo = ref defaultDamage;
    let damageText =
      Js.Unsafe.new_obj
        (Js.Unsafe.js_expr "PIXI.Text")
        [|
          !!(Js.string ("Damage: " ^ string_of_int !damageYouDo)),
          !!(
            Js.Unsafe.obj [|
              ("fontFamily", !!(Js.string "Arial")),
              ("fontSize", !!40),
              ("color", !!16711680),
              ("fill", !!16715792),
              ("align", !!(Js.string "center"))
            |]
          )
        |];
    Js.Unsafe.set damageText "x" !!20;
    Js.Unsafe.set damageText "y" !!20;

    /** Experiment: tap and hold control button to change damage **/
    let _ = {
      let tapOnAblity touchStart this => {
        damageYouDo := touchStart ? (-100) : defaultDamage;
        Js.Unsafe.set damageText "text" !!(Js.string ("Damage: " ^ string_of_int !damageYouDo))
      };

      /** Create controls **/
      let graphics = Js.Unsafe.new_obj (Js.Unsafe.js_expr "PIXI.Graphics") [||];
      ignore @@ Js.Unsafe.meth_call graphics "lineStyle" [|!!2, !!255, !!1|];
      ignore @@ Js.Unsafe.meth_call graphics "beginFill" [|!!16740363, !!1|];
      ignore @@ Js.Unsafe.meth_call graphics "drawRect" [|!!50, !!100, !!350, !!250|];
      ignore @@ Js.Unsafe.meth_call graphics "endFill" [||];
      Js.Unsafe.set graphics "interactive" !!true;
      let text =
        Js.Unsafe.new_obj
          (Js.Unsafe.js_expr "PIXI.Text")
          [|
            !!(Js.string "Controls"),
            !!(
              Js.Unsafe.obj [|
                ("fontFamily", !!(Js.string "Arial")),
                ("fontSize", !!60),
                ("color", !!16711680),
                ("fill", !!16715792),
                ("align", !!(Js.string "center"))
              |]
            )
          |];
      R.Events.onMouse graphics R.Events.MouseDown (tapOnAblity true);
      R.Events.onMouse graphics R.Events.MouseUp (tapOnAblity false);
      R.Events.onMouse graphics R.Events.MouseUpOutside (tapOnAblity false);
      R.Events.onMouse graphics R.Events.TouchStart (tapOnAblity true);
      R.Events.onMouse graphics R.Events.TouchEnd (tapOnAblity false);
      R.Events.onMouse graphics R.Events.TouchEndOutside (tapOnAblity false);
      ignore @@ Js.Unsafe.meth_call controlsStage#raw "addChild" [|!!text|];
      ignore @@ Js.Unsafe.meth_call controlsStage#raw "addChild" [|!!graphics|]
    };

    /** Create game datastructures **/
    let dude2Sprites: ref (DudeMap.t R.Sprite.t) = ref DudeMap.empty;
    let gameState = ref {dudes: []};
    let actionQueue: ref (list actionT) = ref [];
    let animationList: ref (list animationT) = ref [];
    let everythingElseStage = new R.Container.t;
    let dude2AnimationCount = ref DudeMap.empty;
    let animateScreenWhenAttack totalTime::totalTime gameState elapsedTime =>
      if (elapsedTime < totalTime) {
        let angle = sin (elapsedTime /. totalTime *. R.pi *. 10.) /. 40.;
        stage#setRotation angle;
        NotDone gameState
      } else {
        stage#setRotation 0.;
        Done gameState
      };
    let animateClickedDudeShrinkBack
        dude::dude
        bigScale::bigScale
        totalTime::totalTime
        gameState
        elapsedTime =>
      switch (get dude !dude2Sprites) {
      | Some sprite =>
        if (elapsedTime < totalTime) {
          let amount = 1. -. elapsedTime /. totalTime;
          sprite#setScale ((bigScale -. 1.) *. amount +. 1., (bigScale -. 1.) *. amount +. 1.);
          NotDone gameState
        } else {
          sprite#setScale (1., 1.);
          dude2AnimationCount := DudeMap.add dude 0. !dude2AnimationCount;
          Done gameState
        }
      | None => Done gameState
      };

    /** callback triggered when tapping on ally or ennemy **/
    let onOtherDudeTap clickedDude this =>
      if (not clickedDude.friendly) {
        let stackingAnimations = ref 0.;
        switch (get clickedDude !dude2AnimationCount) {
        | Some count => stackingAnimations := count
        | None => stackingAnimations := 0.
        };
        stackingAnimations := !stackingAnimations +. 1.;
        dude2AnimationCount := DudeMap.add clickedDude !stackingAnimations !dude2AnimationCount;
        let deltaHealth = !damageYouDo;
        let action = HealthChange (clickedDude.id, deltaHealth);
        Clientsocket.emit io action;
        /* SIDE EFFECT */
        actionQueue := [action, ...!actionQueue];

        /** Shake animation setup **/
        let callback = animateScreenWhenAttack totalTime::(250. +. !stackingAnimations *. 10.);
        /* SIDE EFFECT */
        animationList := List.filter (fun {dudeID} => dudeID != "random-screen-id") !animationList;
        animationList := [
          {dudeID: "random-screen-id", elapsedTime: 0., callback},
          ...!animationList
        ];

        /** Scaling animation setup and removal **/
        let scalingID = clickedDude.id ^ "scaling";
        animationList := List.filter (fun {dudeID} => dudeID != scalingID) !animationList;
        let callback =
          animateClickedDudeShrinkBack
            dude::clickedDude
            bigScale::(clamp 2. (1.05 +. !stackingAnimations /. 10.))
            totalTime::(150. +. !stackingAnimations *. 50.);
        animationList := [
          {dudeID: scalingID, elapsedTime: 0., callback},
          ...!animationList
        ]
      } else {
        print_endline @@ "probably healing?"
      };

    /** Helper function to diff the current gameState with a new gameState and apply the changes **/
    let resetSprites newGameState dude2Sprites => {
      print_endline "reset state";
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
            /* print_endline @@ "Dude: " ^ dudeToString currentDude; */
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

    /** Function to curry to create an animation of a dude moving to its destination **/
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
            (float_of_int startValue.x +. float_of_int delta.x) *. 200. +. 100.,
            (float_of_int startValue.y +. float_of_int delta.y) *. 200. +. 100.
          );
          Done gameState
        } else {
          sprite#setPositionf (
            (float_of_int startValue.x +. float_of_int delta.x *. (elapsedTime /. totalTime)) *. 200. +. 100.,
            (float_of_int startValue.y +. float_of_int delta.y *. (elapsedTime /. totalTime)) *. 200. +. 100.
          );
          NotDone gameState
        }
      | None => Done gameState
      }
    };

    /** callback that gets called when a local or remote action is triggered **/
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
          print_endline "health change so next resetSprite makes sense";
          resetSprites newGameState dude2Sprites;
          newGameState
        | None =>
          print_endline @@ "Hey eh... This dude doesn't exist '" ^ id ^ "'";
          gameState
        }
      };

    /** callback called when a tap occured not on a special tile **/
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
    ignore @@ Js.Unsafe.meth_call stage#raw "addChild" [|!!damageText|];
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
      mainGameView#render stage;
      controlsView#render controlsStage
    };
    animate 0.;
    Node.m io "emit" [|putStr "join", !!(Js.string yourID)|]
  };
  Js.Unsafe.set (Js.Unsafe.js_expr "window") "onload" !@onLoad
};
