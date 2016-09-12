let module Bindings_utils = {
  open Helpers;
  let require_module s =>
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "require") [|Js.Unsafe.inject (Js.string s)|];

  /** Same as console.log */
  let log obj => Firebug.console##log obj;

  /** Call method of a JavaScript object */
  let m = Js.Unsafe.meth_call;

  /** Inject something as a JS object, be sure its Js.t already,
      functions seemingly exempt */
  let i = Js.Unsafe.inject;

  /** Turn a JavaScript Object into a string */
  let stringify o => Js._JSON##stringify o |> Js.to_string;

  /** Turn an OCaml string into a JavaScript string */
  let to_js_str s => Js.string s |> Js.Unsafe.inject;

  /** Turn a string into a JSON object */
  let json_of_string s => Js._JSON##parse (s |> Js.string);

  /** Create a JavaScript Object out of an alist */
  let obj_of_alist a_l =>
    List.map (fun (key, value) => (key, Js.Unsafe.inject value)) a_l |> Array.of_list |> Js.Unsafe.obj;

  /** Turn JavaScript string array into OCaml list of string */
  let to_string_list g =>
    g |> Js.str_array |> Js.to_array |> Array.map Js.to_string |> Array.to_list;

  /** Turn OCaml list of strings into JavaScript string array */
  let of_string_list g => g |> Array.of_list |> Array.map Js.string |> Js.array;

  /** Get all keys of an Object */
  let keys obj =>
    m (Js.Unsafe.variable "Object") "keys" [|obj|] |> Js.to_array |> Array.map Js.to_string |> Array.to_list;

  /** Call a function for each value of each key in an Object, throw
      away result */
  let for_each_iter f::f obj => keys obj |> List.iter (fun k => f (Js.Unsafe.get obj k));

  /** Call a function for each value of each key in an Object, keep
      result */
  let for_each_map f::f obj => keys obj |> List.map (fun k => f k (Js.Unsafe.get obj k));
  let debug thing field => Firebug.console##log (m (thing <!> field) "toString" [||]);
  type error_arg = Js.opt Js.error;
  let __filename () :Js.t Js.js_string => Js.Unsafe.eval_string "__filename";
  let __dirname () :Js.t Js.js_string => Js.Unsafe.eval_string "__dirname";
  let (|>>) = Js.Unsafe.fun_call;
  let putStr a => Js.Unsafe.inject (Js.string a);
};
