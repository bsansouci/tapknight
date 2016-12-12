TapKnight
---

![14303720_1238615772850309_1534124431_o](https://cloud.githubusercontent.com/assets/4534692/18505729/7d4da830-7a1d-11e6-91c2-bbed50703071.png)

Simple mobile co-op clicker/rogue-like game made entirely in Reason.

You should be able to install simply by cloning and running `npm i`. To start the server run `npm run start` and go to `localhost:3000`. You can use [ngrok](https://ngrok.com) to expose a public URL for your friends to join.

This is a mobile game, so the interactions feel better on a touch screen.
Tap around to move, tap on monsters to kill them, tap on allies to heal them.

## Why is this cool?
The client and server share code which gets compiled to the same JS file which is used for both the client and the server.

We're exploring ways to share a lot more code between the server and the client. For example the list of possible messages that the clinet and server can send to each other is shared among the two. If we go and update that list, the type checker will tell us to also update the relevant parts of the code in the client and the server.

This game is also an exploration for a better high level graphics API. We're currently using quick-and-dirty bindings to Pixi.js, but we plan on using [regl](http://github.com/bsansouci/regl) which is a much lower level API and then design our own to faciliate game dev.
