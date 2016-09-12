/* Open for operators */
let start () => {
  open Helpers;
  let module Node = Nodejs.Bindings_utils;
  let express = Node.require_module "express";
  let path = Node.require_module "path";
  let app = express |>> [||];
  let http = Node.m (Node.require_module "http") "Server" [|!!app|];
  let absolutePath = "/Users/sansouci/Desktop/tapknight";
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
  ignore @@
  Node.m
    io
    "on"
    [|
      putStr "connection",
      !!
        !@(
          fun socket => {
            Node.log "A user connected!";
            Node.m socket "emit" [|putStr "message", putStr "HEYYYY"|]
          }
        )
    |];
  ignore @@
  Node.m
    http "listen" [|!!3000, !!(Js.wrap_meth_callback (fun () => Node.log "asdljhdashkasdhjk"))|];
  ()
};
