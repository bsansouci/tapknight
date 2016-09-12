/* Huge hack to prevent server side code to run in the client :) */

let isServer = Js.to_bool (Js.Unsafe.js_expr {|
  (function(){return typeof require !== "undefined";})()
|});

if (isServer) {
  Server.start ();
} else {
  Client.start ();
}
