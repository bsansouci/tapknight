let module Node = Nodejs.Bindings_utils;

open Helpers;

open Common;

let module SocketComparator = {
  type t = Serversocket.socketT;
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
        id: Js.to_string (Js.Unsafe.js_expr "(Date.now() + 1).toString()"),
        health: 100,
        tint: Js.Unsafe.js_expr "Math.random() * 0xFFFFFF",
        friendly: false
      }
    ]
  };

  /** Handles client disconnecting **/
  let handleDisconnect (socket: Serversocket.socketT) () => {
    print_endline "Some dude disconnected";
    switch (get socket !socket2Dudes) {
    | Some disconnectedDude =>
      socket2Dudes := SocketMap.remove socket !socket2Dudes;
      gameState := removeDude !gameState disconnectedDude;
      SocketMap.iter
        (fun s _ => Serversocket.emitAction socket (RemoveDude disconnectedDude.id)) !socket2Dudes
    | None => print_endline "couldn't find dude :("
    }
  };

  /** Handles actions piped from client to client **/
  let handleAction socket (x: actionT) => {
    print_endline @@ "ready to handle -> " ^ actionToString x;
    switch x {
    | AddDude dude => print_endline "received adddude action, not valid, wtf"
    | MoveDude (id, gameCoord) =>
      switch (getDude !gameState id) {
      | Some dude =>
        let nextGameState = moveDude !gameState dude gameCoord;
        /* Couldn't perform the move if the gameStates are equal */
        if (nextGameState == !gameState) {
          print_endline "not performing move";
          Serversocket.emitAction socket (ResetState !gameState)
        } else {
          print_endline "broadcasting move";
          gameState := nextGameState;
          Node.m (Js.Unsafe.get socket "broadcast") "emit" [|putStr "action", !!x|]
        }
      | None => print_endline @@ "Cannot move dude " ^ id ^ " doesn't exist..."
      }
    | _ => print_endline @@ "handleAction unhandled " ^ actionToString x
    }
  };

  /** HandleJoin is the main entry point. Waits to receive an id and then removes itself. **/
  let handleJoin (socket: Serversocket.socketT) (id: string) => {
    Node.log id;
    switch (getDude !gameState id) {
    | Some dude =>
      socket2Dudes := SocketMap.remove socket !socket2Dudes;
      gameState := removeDude !gameState dude
    | None => ()
    };
    Serversocket.on socket "disconnect" (handleDisconnect socket);
    Serversocket.on socket "action" (handleAction socket);
    let rec recur i prevPos => {
      let isSomeone = List.exists (fun {pos: GameCoord vec} => vec == prevPos) (!gameState).dudes;
      if isSomeone {
        recur (i + 1) {x: prevPos.x + 1, y: prevPos.y}
      } else {
        prevPos
      }
    };
    let pos = recur 0 {x: 2, y: 2};

    /** Creates a new dude, tells everyone else about him. **/
    let yourDude = {
      pos: GameCoord pos,
      id,
      health: 100,
      tint: Js.Unsafe.js_expr "Math.random() * 0xFFFFFF",
      friendly: true
    };
    SocketMap.iter (fun s _ => Serversocket.emitAction s (AddDude yourDude)) !socket2Dudes;

    /** Add the new dude to the gameState and the SocketMap and then send the connection the current
     * gameState.
     **/
    gameState := addDude !gameState yourDude;
    socket2Dudes := SocketMap.add socket yourDude !socket2Dudes;
    Serversocket.emitAction socket (ResetState !gameState)
  };

  /** Handles client connecting **/
  let handleUserConnection (socket: Serversocket.socketT) => {
    print_endline "A user connected!";
    Serversocket.on socket "join" (fun id => handleJoin socket (Js.to_string id))
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
