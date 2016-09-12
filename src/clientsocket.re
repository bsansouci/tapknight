open Js.Unsafe;

open Helpers;

type actionT 'a = {
  typ: string,
  packet: 'a,
};
type ioT;

let start () : ioT => Js.Unsafe.fun_call (Js.Unsafe.js_expr "io") [||];

let on (io : ioT) (cb : actionT 'a => unit) => ignore @@ meth_call io "on" [|putStr "action", !! !@cb|];

let emit (io : ioT) (data : actionT 'a) => ignore @@ meth_call io "emit" [|putStr "action", !!data|];
