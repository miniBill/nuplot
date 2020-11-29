import { Elm } from "./optimized.js";

function init() {
  var worker: Worker = new Worker("/worker.ts");

  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  var app = Elm.UI.init({
    node: node,
    flags: 1,
  });
  app.ports.toWorker.subscribe((json: any) => {
    worker.postMessage(json);
  });
  worker.addEventListener("message", function (e) {
    app.ports.fromWorker.send(e.data);
  });
}

init();
