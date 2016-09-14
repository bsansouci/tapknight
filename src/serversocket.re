open Js.Unsafe;

open Helpers;

type socketT;

let on (socket: socketT) actionName (cb: 'a => unit) =>
  ignore @@ meth_call socket "on" [|putStr actionName, !! !@cb|];

let emitAction (socket: socketT) (data: Common.actionT) =>
  ignore @@ meth_call socket "emit" [|putStr "action", !!data|];

let broadcastAction (socket: socketT) (data: Common.actionT) =>
  ignore @@ meth_call (Js.Unsafe.get socket "broadcast") "emit" [|putStr "action", !!data|];
