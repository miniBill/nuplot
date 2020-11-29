import { Elm } from "./optimized.js";

function init() {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  Elm.UI.init({
    node: node,
    flags: {},
  });
}

init();
