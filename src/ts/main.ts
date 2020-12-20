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

async function retrieveSavedData() {
  try {
    var fromLF = await localForage.getItem(storageKey);
    if (fromLF !== null) return fromLF;
  } catch (e) {}
  return fromLS();
}

const storageKey = "documents";
export async function init(Elm: ElmType) {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  var saved = await retrieveSavedData();

  var app = Elm.UI.init({
    node: node,
    flags: saved,
  });
  app.ports.save.subscribe(async (value) => {
    await localForage.setItem(storageKey, value);
  });
}
