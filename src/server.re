let module Node = Nodejs.Bindings_utils;

open Helpers;

open Common;

type socketT;

let start () => {
  let express = Node.require_module "express";
  let path = Node.require_module "path";
  let app = express |>> [||];
  let http = Node.m (Node.require_module "http") "Server" [|!!app|];
  let absolutePath = Node.m path "join" [|!!(Node.__dirname ()), putStr "..", putStr ".."|];
  ignore @@ Node.m app "use" [|Node.m express "static" [|putStr absolutePath|]|];
  ignore @@
  Node.m
    app
    "get"
    [|
      putStr "/",
      !!
        !@(
          fun req res next =>
            Node.m
              res
              "sendFile"
              [|putStr "index.html", Js.Unsafe.obj [|("root", putStr absolutePath)|]|]
        )
    |];
  let socketio = Node.require_module "socket.io";
  let io = socketio |>> [|http|];
  let otherDudes: ref (list (socketT, option dudeT)) = ref [];
  let monsterDudes: ref (list dudeT) =
    ref
      [
        {
          pos: GameCoord {x: 1, y: 1},
          id: string_of_int (Node.m (Js.Unsafe.js_expr "Date") "now" [||]),
          health: 100,
          tint: Js.Unsafe.js_expr "Math.random() * 0xFFFFFF",
          friendly: false
        },
        {
          pos: GameCoord {x: 3, y: 3},
          id: string_of_int (Node.m (Js.Unsafe.js_expr "Date") "now" [||]),
          health: 100,
          tint: Js.Unsafe.js_expr "Math.random() * 0xFFFFFF",
          friendly: false
        }
      ];
  ignore @@
  Node.m
    io
    "on"
    [|
      putStr "connection",
      !!
        !@(
          fun (socket: socketT) => {
            print_endline "A user connected!";
            ignore @@
            Node.m
              socket
              "on"
              [|
                putStr "disconnect",
                !!
                  !@(
                    fun () => {
                      print_endline "Some dude disconnected";
                      let (_, disconnectedDude) =
                        List.find (fun (s, value) => s == socket) !otherDudes;
                      otherDudes := List.filter (fun (s, value) => s != socket) !otherDudes;
                      switch disconnectedDude {
                      | Some dude =>
                        ignore @@
                        List.map
                          (
                            fun (s, _) =>
                              Node.m s "emit" [|putStr "action", !!(RemoveDude dude.id)|]
                          )
                          !otherDudes
                      | None => ()
                      }
                    }
                  )
              |];
            ignore @@
            Node.m
              socket
              "on"
              [|
                putStr "action",
                !!
                  !@(
                    fun (x: actionT) => {
                      switch x {
                      | AddDude dude =>
                        print_endline "dude-arrived";
                        otherDudes := List.filter (fun (s, _) => s != socket) !otherDudes;
                        otherDudes := !otherDudes @ [(socket, Some dude)]
                      | _ => ()
                      };
                      Node.m (Js.Unsafe.get socket "broadcast") "emit" [|putStr "action", !!x|]
                    }
                  )
              |];
            ignore @@
            List.map
              (
                fun (_, otherDude) =>
                  switch otherDude {
                  | Some dude => Node.m socket "emit" [|putStr "action", !!(AddDude dude)|]
                  | None => ()
                  }
              )
              !otherDudes;
            ignore @@
            List.map
              (
                fun monsterDude => Node.m socket "emit" [|putStr "action", !!(AddDude monsterDude)|]
              )
              !monsterDudes;
            otherDudes := !otherDudes @ [(socket, None)]
          }
        )
    |];
  ignore @@
  Node.m
    http
    "listen"
    [|
      !!(Js.Unsafe.get (Js.Unsafe.get (Js.Unsafe.js_expr "process") "env") "PORT"),
      !!(Js.wrap_meth_callback (fun () => print_endline "asdljhdashkasdhjk"))
    |];
  ()
};
