open Helpers;

open Common;

open Repixi;

open ReasonJs;

open ReSocketIO.Client;

module Socket = Client Common.Action;

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

let defaultHealing = 10;


let createDudeSprite dude onOtherDudeTap => {
  let print = nicePrint "createDudeSprite";
  let dudeTexture =
    if dude.friendly {
      Texture.fromImage uri::"sprites/knight.gif" ()
    } else {
      Texture.fromImage uri::"sprites/dino.gif" ()
    };
  nicePrint "createDudeSprite" dudeTexture;
  let sprite = Sprite.make texture::dudeTexture ();
  sprite##tint#=dude.tint;
  sprite##buttonMode#=true;
  let GameCoord {x, y} = dude.pos;
  print @@ "x:" ^ string_of_int x ^ " y: " ^ string_of_int y;
  sprite##x#=(float_of_int (x * 200 + 100));
  sprite##y#=(float_of_int (y * 200 + 100));
  sprite##interactive#=true;
  Events.on sprite Events.MouseDown (onOtherDudeTap dude);
  Events.on sprite Events.TouchStart (onOtherDudeTap dude);
  /* This has to be mutation. `anchor` is an ObservablePoint, not a Point. */
  sprite##anchor##x#=0.5;
  sprite##anchor##y#=0.5;
  sprite
};

let onLoad () => {
  let print = nicePrint "onLoad";
  let io = Socket.create ();
  let yourID = string_of_int (ReasonJs.Date.now ());
  let width = ReasonJs.Window.innerWidth ();
  let height = ReasonJs.Window.innerHeight ();
  let minSize = min (width - 30) height;
  let minSizef = float_of_int minSize;
  let mainGameView = Renderer.autoDetectRenderer width::minSize height::minSize ();
  mainGameView##backgroundColor#="0xFFFFFF";
  ReasonJs.Document.appendChild (ReasonJs.Document.getElementById "main") mainGameView##view;
  let controlsView = Renderer.autoDetectRenderer width::minSize height::(height - minSize) ();
  controlsView##backgroundColor#="0xFFFFFF";
  ReasonJs.Document.appendChild (ReasonJs.Document.getElementById "main") controlsView##view;

  /** Create different top level containers **/
  let stage = Container.make ();
  let controlsStage = Container.make ();
  let scale = float_of_int minSize /. (200. *. 5.);
  stage##scale#=(Point.make scale scale);
  stage##pivot#=(Point.make (minSizef /. 2.) (minSizef /. 2.));
  stage##x#=(minSizef /. 2.);
  stage##y#=(minSizef /. 2.);
  controlsStage##scale#=(Point.make scale scale);
  let damageYouDo = ref defaultDamage;
  let healingYouDo = ref defaultHealing;
  let damageText =
    Text.make
      ("Damage: " ^ string_of_int !damageYouDo)
      (
        Text.options
          fontFamily::"Arial" fontSize::40 color::16711680 fill::16715792 align::"center" ()
      );
  damageText##x#=20.;
  damageText##y#=20.;

  /** Create game datastructures **/
  let dude2Sprites: ref (DudeMap.t Sprite.t) = ref DudeMap.empty;
  let gameState = ref {dudes: [], grid: [||]};
  let actionQueue: ref (list Action.dataT) = ref [];
  let animationList: ref (list animationT) = ref [];
  let everythingElseStage = Container.make ();
  let dude2AnimationCount: ref (DudeMap.t float) = ref DudeMap.empty;
  let animateScreenWhenAttack totalTime::totalTime gameState elapsedTime =>
    if (elapsedTime < totalTime) {
      let angle = sin (elapsedTime /. totalTime *. pi *. 10.) /. 40.;
      stage##rotation#=angle;
      NotDone gameState
    } else {
      stage##rotation#=0.;
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
        sprite##scale#=(
                         Point.make
                           ((bigScale -. 1.) *. amount +. 1.) ((bigScale -. 1.) *. amount +. 1.)
                       );
        NotDone gameState
      } else {
        sprite##scale#=(Point.make 1. 1.);
        dude2AnimationCount := DudeMap.add dude 0. !dude2AnimationCount;
        Done gameState
      }
    | None => Done gameState
    };

  /** callback triggered when tapping on ally or ennemy **/
  let onOtherDudeTap clickedDude _ => {
    let print = nicePrint "onOtherDudeTap";
    if (not clickedDude.friendly) {
      let stackingAnimations = ref 0.;
      switch (get clickedDude !dude2AnimationCount) {
      | Some count => stackingAnimations := count
      | None => stackingAnimations := 0.
      };
      stackingAnimations := !stackingAnimations +. 1.;
      dude2AnimationCount := DudeMap.add clickedDude !stackingAnimations !dude2AnimationCount;
      let deltaHealth = !damageYouDo;
      let action = Action.HealthChange (clickedDude.id, deltaHealth);
      Socket.emit io Action.Action action;
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
      animationList := [{dudeID: scalingID, elapsedTime: 0., callback}, ...!animationList]
    } else {
      print @@ "probably healing?";
      let deltaHealth = !healingYouDo;
      let action = Action.HealthChange (clickedDude.id, deltaHealth);
      Socket.emit io Action.Action action
    }
  };

  /** Helper function to diff the current gameState with a new gameState and apply the changes **/
  let resetSprites newGameState dude2Sprites => {
    let print = nicePrint "resetSprites";
    print "reset state";
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
    print @@ "dudes to remove " ^ string_of_int (List.length !dudesToRemove);
    List.iter
      (
        fun (dudeToRemove, spriteToRemove) => {
          /* SIDE EFFECT */
          Container.removeChild everythingElseStage Sprite spriteToRemove;
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
          | Some sprite =>
            print "you found yourself!";
            /* Don't do anything */ ()
          | None =>
            /* SIDE EFFECT */
            print @@ "Adding dude: " ^ currentDude.id;
            let dudeSprite = createDudeSprite currentDude onOtherDudeTap;
            nicePrint "resetSprites" dudeSprite;
            dude2Sprites := DudeMap.add currentDude dudeSprite !dude2Sprites;
            Container.addChild parent::everythingElseStage Sprite child::dudeSprite ()
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
        sprite##x#=((float_of_int startValue.x +. float_of_int delta.x) *. 200. +. 100.);
        sprite##y#=((float_of_int startValue.y +. float_of_int delta.y) *. 200. +. 100.);
        Done gameState
      } else {
        sprite##x#=(
                     (
                       float_of_int startValue.x +.
                       float_of_int delta.x *. (elapsedTime /. totalTime)
                     ) *. 200. +. 100.
                   );
        sprite##y#=(
                     (
                       float_of_int startValue.y +.
                       float_of_int delta.y *. (elapsedTime /. totalTime)
                     ) *. 200. +. 100.
                   );
        NotDone gameState
      }
    | None => Done gameState
    }
  };

  /** callback called when a tap occured not on a special tile **/
  let onButtonDown gameState this => {
    let print = nicePrint "onButtonDown";
    nicePrint "onButtonDown" this;
    switch (getDude !gameState yourID) {
    | Some yourDude =>
      let maybeAnimation = find (fun {dudeID} => dudeID == yourDude.id) !animationList;
      switch maybeAnimation {
      | Some animation => ()
      | None =>
        let x: int = int_of_float (this##x /. 200.);
        let y: int = int_of_float (this##y /. 200.);
        /* If tile contains monster, something else happens */
        let GameCoord {x: centerX, y: centerY} = yourDude.pos;
        let (dx, dy) = (x - centerX, y - centerY);
        let delta =
          if (abs dx > abs dy) {
            dx > 0 ? {x: 1, y: 0} : {x: (-1), y: 0}
          } else {
            dy > 0 ? {x: 0, y: 1} : {x: 0, y: (-1)}
          };
        let action = Action.MoveDude (yourDude.id, GameCoord delta);
        Socket.emit io Action.Action action;
        actionQueue := [action, ...!actionQueue]
      }
    | None =>
      print "Your dude got killed";
      assert false
    }
  };

  /** Resets the whole grid. **/
  let resetGrid {grid} => {
    let print = nicePrint "resetGrid";
    /* TODO: do some cleanup! */
    gameState := {...!gameState, grid};
    let emptyTexture = Texture.fromImage uri::"sprites/bg-empty.gif" ();
    let filledTexture = Texture.fromImage uri::"sprites/bg-filled.gif" ();
    let height = Array.length grid - 1;
    let width = Array.length grid.(0) - 1;
    for i in 0 to width {
      for j in 0 to height {
        switch grid.(j).(i) {
        | Empty =>
          let tile = Sprite.make texture::emptyTexture ();
          tile##buttonMode#=true;
          tile##x#=(float_of_int i *. 200.);
          tile##y#=(float_of_int j *. 200.);
          tile##interactive#=true;
          Container.addChild parent::everythingElseStage Sprite child::tile ()
        | Filled =>
          let tile = Sprite.make texture::filledTexture ();
          tile##buttonMode#=true;
          tile##x#=(float_of_int i *. 200.);
          tile##y#=(float_of_int j *. 200.);
          tile##interactive#=true;
          Events.on tile Events.MouseDown (onButtonDown gameState);
          Events.on tile Events.TouchStart (onButtonDown gameState);
          Container.addChild parent::everythingElseStage Sprite child::tile ()
        }
      }
    }
  };

  /** callback that gets called when a local or remote action is triggered **/
  let handleAction (action: Action.dataT) (gameState: gameStateT) => {
    let print = nicePrint "handleAction";
    switch action {
    | Action.ResetState newGameState =>
      print "received reset gamestate";
      resetGrid newGameState;
      resetSprites newGameState dude2Sprites;
      newGameState
    | Action.RemoveDude id =>
      print "dude-left";
      switch (getDude gameState id) {
      | Some dude =>
        switch (get dude !dude2Sprites) {
        | Some sprite =>
          /* SIDE EFFECT */
          Container.removeChild everythingElseStage Sprite sprite;
          removeDude gameState dude
        | None => gameState
        }
      | None => assert false
      }
    | Action.AddDude dude =>
      print @@ "dude-arrived " ^ dudeToString dude;
      let dudeSprite = createDudeSprite dude onOtherDudeTap;
      dude2Sprites := DudeMap.add dude dudeSprite !dude2Sprites;
      Container.addChild parent::everythingElseStage Sprite child::dudeSprite ();
      addDude gameState dude
    | Action.MoveDude (id, delta) =>
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
        print @@ "Hey eh... This dude doesn't exist '" ^ id ^ "'";
        gameState
      }
    | Action.HealthChange (id, deltaHealth) =>
      switch (getDude gameState id) {
      | Some dude =>
        let newGameState = changeHealth gameState dude deltaHealth;
        print "health change so next resetSprite makes sense";
        resetSprites newGameState dude2Sprites;
        newGameState
      | None =>
        print @@ "Hey eh... This dude doesn't exist '" ^ id ^ "'";
        gameState
      }
    }
  };
  Container.addChild parent::stage Container child::everythingElseStage ();
  Container.addChild parent::stage Text child::damageText ();
  Socket.on io Action.Action (fun message => actionQueue := [message, ...!actionQueue]);
  let prevTime = ref 0.;
  let rec animate time => {
    let deltaTime = time -. !prevTime;
    prevTime := time;
    requestAnimationFrame animate;
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
        everythingElseStage##x#=(-. dudeSprite##x +. 400.);
        everythingElseStage##y#=(-. dudeSprite##y +. 400.)
      | None => assert false
      }
    | None => /* You don't have your own dude yet... I guess... */ ()
    };
    Renderer.render mainGameView stage;
    Renderer.render controlsView controlsStage
  };
  requestAnimationFrame animate;
  Socket.emit io Action.Join yourID
};

ReasonJs.Window.addEventListener "load" onLoad;
