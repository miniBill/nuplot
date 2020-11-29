import { Elm } from "./elm/Worker.elm";

const ctx: Worker = self as any;

// @ts-ignore Allow Elm to work in a Worker
ctx.document = { body: {} };

var worker = Elm.Worker.init();
worker.ports.fromWorker.subscribe((msg) => ctx.postMessage(msg));

self.addEventListener("message", function (e) {
  worker.ports.toWorker.send(e.data);
});
