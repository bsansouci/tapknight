/* Open for operators */
type socketT;

let start () => {
  open Helpers;
  let module Node = Nodejs.Bindings_utils;
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
  let otherDudes: ref (list (socketT, option Client.dudeT)) = ref [];
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
                              Node.m
                                s
                                "emit"
                                [|
                                  putStr "action",
                                  !!Clientsocket.{typ: "dude-left", packet: dude}
                                |]
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
                    fun (x: Clientsocket.actionT 'a) => {
                      switch x.typ {
                      | "dude-arrived" =>
                        print_endline "dude-arrived";
                        let dude: Client.dudeT = x.packet;
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
                  | Some dude =>
                    Node.m
                      socket
                      "emit"
                      [|putStr "action", !!Clientsocket.{typ: "dude-arrived", packet: dude}|]
                  | None => ()
                  }
              )
              !otherDudes;
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
