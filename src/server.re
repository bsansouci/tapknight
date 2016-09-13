let module Node = Nodejs.Bindings_utils;

open Helpers;

open Common;

type socketT;

let module SocketComparator = {
  type t = socketT;
  /* This won't work in ocaml (it only works in JS) */
  let compare a b => {
    let id1 = Js.to_string (Js.Unsafe.get a "id");
    let id2 = Js.to_string (Js.Unsafe.get b "id");
    String.compare id1 id2
  };
};

let module SocketMap = Map.Make SocketComparator;

let get key map =>
  try (Some (SocketMap.find key map)) {
  | Not_found => None
  };

let start () => {
  let express = Node.require_module "express";
  let path = Node.require_module "path";
  let app = express |>> [||];
  let http = Node.m (Node.require_module "http") "Server" [|!!app|];
  let absolutePath = Node.m path "join" [|!!(Node.__dirname ()), putStr "..", putStr ".."|];
  ignore @@ Node.m app "use" [|Node.m express "static" [|putStr absolutePath|]|];

  /** Function handling main get request **/
  let mainHandleRequest req res next =>
    Node.m res "sendFile" [|putStr "index.html", Js.Unsafe.obj [|("root", putStr absolutePath)|]|];
  ignore @@ Node.m app "get" [|putStr "/", !! !@mainHandleRequest|];
  let socketio = Node.require_module "socket.io";
  let io = socketio |>> [|http|];
  let socket2Dudes: ref (SocketMap.t dudeT) = ref SocketMap.empty;
  let gameState = ref {
    dudes: [
      {
        pos: GameCoord {x: 1, y: 1},
        id: Js.to_string (Js.Unsafe.js_expr "Date.now().toString()"),
        health: 100,
        tint: Js.Unsafe.js_expr "Math.random() * 0xFFFFFF",
        friendly: false
      },
      {
        pos: GameCoord {x: 3, y: 3},
        id: Js.to_string (Js.Unsafe.js_expr "Date.now().toString()"),
        health: 100,
        tint: Js.Unsafe.js_expr "Math.random() * 0xFFFFFF",
        friendly: false
      }
    ]
  };

  /** Handles client disconnecting **/
  let handleDisconnect (socket: socketT) () => {
    print_endline "Some dude disconnected";
    switch (get socket !socket2Dudes) {
    | Some disconnectedDude =>
      socket2Dudes := SocketMap.remove socket !socket2Dudes;
      gameState := removeDude !gameState disconnectedDude;
      SocketMap.iter
        (fun s _ => Node.m s "emit" [|putStr "action", !!(RemoveDude disconnectedDude.id)|])
        !socket2Dudes
    | None => print_endline "couldn't find dude :("
    }
  };

  /** Handles actions piped from client to client **/
  let handleAction socket (x: actionT) => {
    switch x {
    | AddDude dude => print_endline "received adddude action, not valid, wtf"
    | _ => ()
    };
    Node.m (Js.Unsafe.get socket "broadcast") "emit" [|putStr "action", !!x|]
  };

  /** HandleJoin is the main entry point. Waits to receive an id and then removes itself. **/
  let handleJoin (socket : socketT) (id : string) => {
    Node.log id;
    switch (getDude !gameState id) {
    | Some dude =>
      socket2Dudes := SocketMap.remove socket !socket2Dudes;
      gameState := removeDude !gameState dude
    | None => ()
    };
    ignore @@ Node.m socket "on" [|putStr "disconnect", !! !@(handleDisconnect socket)|];
    ignore @@ Node.m socket "on" [|putStr "action", !! !@(handleAction socket)|];

    /** Creates a new dude, tells everyone else about him. **/
    let yourDude = {
      pos: GameCoord {x: 2, y: 2},
      id,
      health: 100,
      tint: Js.Unsafe.js_expr "Math.random() * 0xFFFFFF",
      friendly: true
    };
    SocketMap.iter
      (fun s _ => Node.m s "emit" [|putStr "action", !!(AddDude yourDude)|]) !socket2Dudes;

    /** Add the new dude to the gameState and the SocketMap and then send the connection the current
     * gameState.
     **/
    gameState := addDude !gameState yourDude;
    socket2Dudes := SocketMap.add socket yourDude !socket2Dudes;
    Node.m socket "emit" [|putStr "action", !!(ResetState !gameState)|]
  };

  /** Handles client connecting **/
  let handleUserConnection (socket: socketT) => {
    print_endline "A user connected!";
    ignore @@ Node.m socket "on" [|putStr "join", !! !@(fun id => handleJoin socket (Js.to_string id))|]
  };
  ignore @@ Node.m io "on" [|putStr "connection", !! !@handleUserConnection|];
  ignore @@
  Node.m
    http
    "listen"
    [|
      !!(Js.Unsafe.get (Js.Unsafe.get (Js.Unsafe.js_expr "process") "env") "PORT"),
      !!(Js.wrap_meth_callback (fun () => print_endline "Server running"))
    |];
  ()
};
