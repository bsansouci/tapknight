open Js.Unsafe;

open Helpers;

type ioT;

let start () : ioT => Js.Unsafe.fun_call (Js.Unsafe.js_expr "io") [||];

let on (io : ioT) (cb : Common.actionT => unit) => ignore @@ meth_call io "on" [|putStr "action", !! !@cb|];

let emit (io : ioT) (data : Common.actionT) => ignore @@ meth_call io "emit" [|putStr "action", !!data|];
