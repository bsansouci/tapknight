let start () => {
  /* Module alias is 10/10 better than open */
  let module M = {
    let pixi = Js.Unsafe.js_expr "PIXI";
  };
  let module R = Repixi.Repixi.Init M;
  open Js.Unsafe;
  open Helpers;
  let io = Js.Unsafe.fun_call (Js.Unsafe.js_expr "io") [||];
  ignore @@
  meth_call
    io "on" [|putStr "message", !! !@(fun x => Nodejs.Bindings_utils.log (Js.string "asd"))|];
  let renderer = R.autoDetectRenderer width::800 height::600;
  R.Dom.appendToBody renderer#view;
  let stage = new R.Container.t;
  let background = R.Sprite.fromImage uri::"_assets/button_test_BG.jpg";
  background#setWidth renderer#width;
  background#setHeight renderer#height;
  stage#addChild background;
  let textureButton = R.Texture.fromImage uri::"_assets/button.png";
  let textureButtonDown = R.Texture.fromImage uri::"_assets/buttonDown.png";
  let textureButtonOver = R.Texture.fromImage uri::"_assets/buttonOver.png";
  let buttons = ref [];
  let buttonPositions = [|175, 75, 655, 75, 410, 325, 150, 465, 685, 445|];
  let onButtonDown this => {
    this#setIsDown true;
    this#setTexture textureButtonDown;
    this#setAlpha 1.0
  };
  let onButtonUp this => {
    this#setIsDown false;
    if this#isOver {
      this#setTexture textureButtonOver
    } else {
      this#setTexture textureButton
    }
  };
  let onButtonOver this => {
    this#setIsOver true;
    if (not this#isDown) {
      this#setTexture textureButtonOver
    }
  };
  let onButtonOut this => {
    this#setIsOver false;
    if (not this#isDown) {
      this#setTexture textureButton
    }
  };
  for i in 0 to 4 {
    let button = (new R.Sprite.t) textureButton;
    button#setButtonMode true;
    button#setAnchor (0.5, 0.5);
    button#setPosition (buttonPositions.(i * 2), buttonPositions.(i * 2 + 1));
    button#setInteractive true;
    {
      open R.Events;
      button#on MouseDown onButtonDown;
      button#on TouchStart onButtonDown;
      button#on MouseUp onButtonUp;
      button#on TouchEnd onButtonUp;
      button#on MouseUpOutside onButtonUp;
      button#on TouchEndOutside onButtonUp;
      button#on MouseOver onButtonOver;
      button#on MouseOut onButtonOut
    };
    stage#addChild button;
    buttons := !buttons @ [button]
  };
  (List.nth !buttons 0)#setScale (1.2, 1.2);
  (List.nth !buttons 2)#setRotation (R.pi /. 10.);
  (List.nth !buttons 3)#setScale (0.8, 0.8);
  (List.nth !buttons 4)#setScale (0.8, 1.2);
  (List.nth !buttons 4)#setRotation R.pi;
  let rec animate () => {
    Dom_html._requestAnimationFrame (Js.wrap_callback animate);
    renderer#render stage
  };
  animate ()
};
