import { ElmType } from "../elm/UI.elm";
import { NuPlot } from "./nuplot";
import { MathJaxElement } from "./mathjax";

customElements.define("nu-plot", NuPlot);
customElements.define("math-jax", MathJaxElement);

export function init(Elm: ElmType) {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  var saved: { [key: string]: string } = {};
  for (var i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (key === null) continue;
    const value = localStorage.getItem(key);
    if (value === null) continue;
    saved[key] = value;
  }

  var app = Elm.UI.init({
    node: node,
    flags: saved,
  });
  app.ports.save.subscribe((param) => {
    const { key, value } = param;
    localStorage.setItem(key, value);
  });
}
