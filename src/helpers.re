let (!@) f => Js.wrap_callback f;

let (!!) o => Js.Unsafe.inject o;


/** Get the field of a JavaScript Object */
let (<!>) obj field => Js.Unsafe.get obj field;

let (|>>) = Js.Unsafe.fun_call;

let putStr a => Js.Unsafe.inject (Js.string a);
