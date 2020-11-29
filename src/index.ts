import { Elm } from "./optimized.js";
import { NuPlot } from "./nuplot";

function init() {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  customElements.define("nu-plot", NuPlot);

  Elm.UI.init({
    node: node,
    flags: {},
  });
}

init();
