open Rexpress.Express;

let __dirname: Js.undefined string = [%bs.node __dirname];

let app = Express.express ();

type pathT;

external path : pathT = "" [@@bs.module];

external join : pathT => Js.undefined string => array string => string = "join" [@@bs.send] [@@bs.splice];

Express.use app (Express.static path::(join path __dirname [|"..", "..", ".."|]));

module Http = {
  type http;
  external make : Express.t => http = "Server" [@@bs.module "http"];
  external listen : http => int => (unit => unit) => unit = "" [@@bs.send];
};

let http = Http.make app;

Express.get app "/" (fun req res => Response.sendFile res "index.html" {"root": __dirname});

open ReSocketIO.Server;

module Server = Server Common.Action;

module SocketComparator = {
  type t = ReSocketIO.Server.socketT;
  let compare a b => String.compare (Server.Socket.id a) (Server.Socket.id b);
};

module SocketMap = Map.Make SocketComparator;

let get key map =>
  try (Some (SocketMap.find key map)) {
  | Not_found => None
  };

let io = Server.createWithHttp http;

open Common;

open ReasonJs;

let socket2Dudes: ref (SocketMap.t dudeT) = ref SocketMap.empty;

let grid = [|
  [|Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Empty, Filled, Empty, Filled, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Empty, Filled, Filled, Empty, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Empty, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled|],
  [|Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled, Filled|]
|];

let gameState =
  ref {
    dudes: [
      {
        pos: GameCoord {x: 1, y: 1},
        id: makeID (),
        health: 100,
        tint: int_of_float (ReasonJs.Math.random () *. 16777215.),
        friendly: false
      },
      {
        pos: GameCoord {x: 3, y: 3},
        id: makeID () ^ "asd", /* concatenating something just in case this runs in the same ms as the one above */
        health: 100,
        tint: int_of_float (ReasonJs.Math.random () *. 16777215.),
        friendly: false
      }
    ],
    grid
  };

/* let generateNewMonster () => {
     let numberOfPeople = List.length (List.filter (fun dude => dude.friendly) (!gameState).dudes);
     let numberOfMonsters =
       List.length (List.filter (fun monster => not monster.friendly) (!gameState).dudes);
     print_endline @@
     "Should I generate a monster? " ^
     string_of_int (10 * numberOfPeople) ^ " vs " ^ string_of_int numberOfMonsters;
     if (numberOfMonsters < 10 * numberOfPeople) {
       let generatedVec = ref {x: Random.int 40, y: Random.int 40};
       while (List.exists (fun {pos: GameCoord vec} => vec == !generatedVec) (!gameState).dudes) {
         generatedVec := {x: Random.int 40, y: Random.int 40}
       };
       let newMonster = {
         pos: GameCoord !generatedVec,
         id: makeID (),
         health: 100,
         tint: int_of_float (ReasonJs.Math.random () *. 16777215.),
         friendly: false
       };
       gameState := {...!gameState, dudes: [newMonster, ...(!gameState).dudes]};
       Server.emit io Action.Action (Action.AddDude newMonster)
     }
   }; */
/* ReasonJs.setInterval generateNewMonster 2000; */

/** Handles client disconnecting **/
let handleDisconnect socket () => {
  let print = nicePrint "handleDisconnect";
  print "Some dude disconnected";
  switch (get socket !socket2Dudes) {
  | Some disconnectedDude =>
    socket2Dudes := SocketMap.remove socket !socket2Dudes;
    gameState := removeDude !gameState disconnectedDude;
    SocketMap.iter
      (fun s _ => Server.Socket.emit s Action.Action (Action.RemoveDude disconnectedDude.id))
      !socket2Dudes
  | None => print "couldn't find dude :("
  }
};


/** HandleJoin is the main entry point. Waits to receive an id and then removes itself. **/
let handleJoin socket id => {
  let print = nicePrint "handleJoin";
  print id;
  switch (getDude !gameState id) {
  | Some dude =>
    socket2Dudes := SocketMap.remove socket !socket2Dudes;
    gameState := removeDude !gameState dude
  | None => ()
  };
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
    tint: int_of_float (ReasonJs.Math.random () *. 16777215.),
    friendly: true
  };
  print @@ "created dude -> " ^ dudeToString yourDude;
  SocketMap.iter
    (fun s _ => Server.Socket.emit s Action.Action (Action.AddDude yourDude)) !socket2Dudes;

  /**
   * Add the new dude to the gameState and the SocketMap and then send the connection the current
   * gameState.
   **/
  gameState := addDude !gameState yourDude;
  socket2Dudes := SocketMap.add socket yourDude !socket2Dudes;
  Server.Socket.emit socket Action.Action (Action.ResetState !gameState)
};


/** Handles actions piped from client to client **/
let handleAction socket receivedAction => {
  let print = nicePrint "handleAction";
  print @@ "ready to handle -> " ^ actionToString receivedAction;
  let maybeNewGameState =
    switch receivedAction {
    | Action.AddDude dude =>
      print "received adddude action, not valid, wtf";
      None
    | Action.HealthChange (id, deltaHealth) =>
      switch (getDude !gameState id) {
      | Some dude =>
        Server.Socket.broadcast socket Action.Action receivedAction;
        Some (changeHealth !gameState dude deltaHealth)
      | None =>
        print "not performing health change, dude probably dead already";
        None
      }
    | Action.MoveDude (id, gameCoord) =>
      print "got movedude";
      nicePrint "handleAction" id;
      switch (getDude !gameState id) {
      | Some dude =>
        switch (moveDude !gameState dude gameCoord) {
        | Some nextGameState =>
          Server.Socket.broadcast socket Action.Action receivedAction;
          Some nextGameState
        | None =>
          print "not performing move";
          None
        }
      | None =>
        print @@ "Cannot move dude " ^ id ^ " doesn't exist...";
        None
      }
    | _ =>
      print @@ "handleAction unhandled " ^ actionToString receivedAction;
      None
    };
  switch maybeNewGameState {
  | Some newGameState => gameState := newGameState
  | None =>
    print "Somthing happened and we're resetting the state";
    Server.Socket.emit socket Action.Action (Action.ResetState !gameState)
  }
};


/** Handles client connecting **/
let handleUserConnection socket => {
  let print = nicePrint "handleUserConnection";
  print "A user connected!";
  /* Server.Socket.on socket
     (
       fun t data =>
         switch t {
           | Action.Action => handleAction socket data
           | Action.Join => handleJoin socket data
           | Action.Disconnect => handleDisconnect socket ()
         }
       ); */
  Server.Socket.on socket Action.Join (handleJoin socket);
  Server.Socket.on socket Action.Disconnect (handleDisconnect socket);
  Server.Socket.on socket Action.Action (handleAction socket)
};

Server.onConnect io handleUserConnection;

Http.listen http 3000 (fun () => print_endline "listening on *:3000");
