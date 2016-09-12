/* Open for operators */
let start () => {
  open Helpers;
  let module Node = Nodejs.Bindings_utils;
  let express = Node.require_module "express";
  let path = Node.require_module "path";
  let app = express |>> [||];
  let http = Node.m (Node.require_module "http") "Server" [|!!app|];
  let absolutePath = (Node.m path "join" [|!!(Node.__dirname ()), putStr "..", putStr ".."|]);
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
  let otherDudes: ref (list Client.dudeT) = ref [];
  ignore @@
  Node.m
    io
    "on"
    [|
      putStr "connection",
      !!
        !@(
          fun socket => {
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
                      print_endline "Some dude called";
                      let dude: Client.dudeT = Js.Unsafe.get socket "dude";
                      otherDudes :=
                        List.filter (fun (value: Client.dudeT) => value.id != dude.id) !otherDudes
                    }
                  )
              |]
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
                      | "new-dude-arrived" =>
                        print_endline "new-dude-arrived";
                        let dude: Client.dudeT = x.packet;
                        /* Shove the dude inside the socket for disconnect event. Deal with it. */
                        Js.Unsafe.set socket "dude" dude;
                        otherDudes := !otherDudes @ [dude]
                      | _ => ()
                      };
                      Node.m (Js.Unsafe.get socket "broadcast") "emit" [|putStr "action", !!x|]
                    }
                  )
              |];
            List.map
              (
                fun otherDude =>
                  Node.m
                    socket
                    "emit"
                    [|
                      putStr "action",
                      !!Clientsocket.{typ: "new-dude-arrived", packet: otherDude}
                    |]
              )
              !otherDudes
          }
        )
    |];
  ignore @@
  Node.m
    http
    "listen"
    [|!!(Js.Unsafe.get (Js.Unsafe.get (Js.Unsafe.js_expr "process") "env") "PORT"), !!(Js.wrap_meth_callback (fun () => print_endline "asdljhdashkasdhjk"))|];
  ()
};
