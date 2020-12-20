import { ElmType } from "../elm/UI.elm";
import { NuPlot } from "./nuplot";
import { MathJaxElement } from "./mathjax";
import localForage from "localforage";

window.MathJax = {
  // @ts-ignore
  startup: {
    typeset: false,
  },
};

customElements.define("nu-plot", NuPlot);
customElements.define("math-jax", MathJaxElement);

const storageKey = "documents";

function fromLS() {
  var saved: { [key: string]: string } = {};
  for (var i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (key === null) continue;
    const value = localStorage.getItem(key);
    if (value === null) continue;
    saved[key] = value;
  }
  return saved;
}

function innerInit(Elm: ElmType, saved: any) {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  var app = Elm.UI.init({
    node: node,
    flags: saved,
  });
  app.ports.save.subscribe((value) => {
    localForage.setItem(storageKey, value);
  });
}

export function init(Elm: ElmType) {
  localForage
    .getItem(storageKey)
    .then((saved) => innerInit(Elm, saved))
    .catch(() => innerInit(Elm, fromLS()));
}
