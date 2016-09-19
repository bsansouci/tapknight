external pi_2 : float = "PIXI.PI_2" [@@bs.val];
let pi = pi_2 /. 2.;

external requestAnimationFrame : (float => unit) => unit = "requestAnimationFrame" [@@bs.val];

let module ObservablePoint = {
  type t = Js.t <x [@bs.set] : float, y [@bs.set] : float>;
};

let module Point = {
  type t = Js.t <x [@bs.set] : float, y [@bs.set] : float>;
  external make : float => float => t = "PIXI.Point" [@@bs.new];
  external set : t => float => float => unit = "set" [@@bs.send];
};

let module Events = {
  type mouseEventT =
    | MouseDown
    | MouseUp
    | MouseUpOutside
    | MouseOver
    | MouseOut
    | TouchStart
    | TouchEnd
    | TouchEndOutside;
  type keyboardEventT =
    | KeyDown
    | KeyUp
    | KeyPress;
  type mouseOrKeyboard =
    | Mouse mouseEventT
    | Keyboard keyboardEventT;
  let eventTypeToString evtType =>
    switch evtType {
    | Mouse e =>
      switch e {
      | MouseDown => "mousedown"
      | MouseUp => "mouseup"
      | MouseUpOutside => "mouseupoutside"
      | MouseOver => "mouseover"
      | MouseOut => "mouseout"
      | TouchStart => "touchstart"
      | TouchEnd => "touchend"
      | TouchEndOutside => "touchendoutside"
      }
    | Keyboard e =>
      switch e {
      | KeyDown => "keydown"
      | KeyUp => "keyup"
      | KeyPress => "keypress"
      }
    };
  external _on : 'a => string => ('a => unit [@bs]) => unit = "on" [@@bs.send];
  let on element (evt: mouseEventT) (cb: 'a => unit) =>
    _on element (eventTypeToString (Mouse evt)) (fun _ => cb element);
  /* let onKeyboard (evt: keyboardEventT) (cb: any => unit) =>
     ignore @@
     meth_call
       (variable "window")
       "addEventListener"
       [|inject (Js.string (stringForEventType (Keyboard evt))), inject (Js.wrap_callback cb)|]; */
};

let module Texture = {
  type t;
  external fromImage : uri::string => unit => t = "PIXI.Texture.fromImage" [@@bs.val];
};

let module Sprite = {
  type t =
    Js.t <
      anchor [@bs.set] : ObservablePoint.t,
      buttonMode [@bs.set] : bool,
      interactive [@bs.set] : bool,
      scale [@bs.set] : Point.t,
      tint [@bs.set] : int,
      x [@bs.set] : float,
      y [@bs.set] : float
    >;
  external make : texture::Texture.t => unit => t = "PIXI.Sprite" [@@bs.new];
};

let module Text = {
  type t = Js.t <x [@bs.set] : float, y [@bs.set] : float>;
  type optionsT;
  external options : fontFamily::string =>
                     fontSize::int =>
                     color::int =>
                     fill::int =>
                     align::string =>
                     unit =>
                     optionsT = "" [@@bs.obj];
  external make : string => optionsT => t = "PIXI.Text" [@@bs.new];
};

type kind 'container =
  | Sprite :kind Sprite.t
  | Container :kind 'container
  | Text :kind Text.t;

let module Container = {
  type t =
    Js.t <
      anchor [@bs.set] : Point.t,
      pivot [@bs.set] : Point.t,
      rotation [@bs.set] : float,
      scale [@bs.set] : Point.t,
      x [@bs.set] : float,
      y [@bs.set] : float
    >;
  external make : unit => t = "PIXI.Container" [@@bs.new];
  external removeChild : t => (kind 'a) [@bs.ignore] => 'a => unit = "removeChild" [@@bs.send];
  external addChild : parent::t => (kind 'a) [@bs.ignore] => child::'a => unit => unit = "addChild" [@@bs.send];
};

/* This has to be below container to accept the type. The better way to do it would be to define Functors
 * which gets setup in a dummy file that does module aliases:
 * let module Renderer = RendererMaker(Container);
 * */
let module Renderer = {
  type htmlCanvasElementT;
  type optionsT;
  external options : view::htmlCanvasElementT? =>
                     transparent::bool? =>
                     antialias::bool? =>
                     preserveDrawingBuffer::bool? =>
                     resolution::int? =>
                     unit =>
                     optionsT = "" [@@bs.obj];
  type t = Js.t <backgroundColor [@bs.set] : string, view [@bs.get] : htmlCanvasElementT>;
  external autoDetectRenderer : width::int => height::int => options::optionsT? => unit => t = "PIXI.autoDetectRenderer" [@@bs.val];
  external render : t => Container.t => unit = "render" [@@bs.send];
};
