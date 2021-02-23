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

function loadMathJax() {
  var script = document.createElement("script");
  script.async = true;
  script.src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js";

  document.head.appendChild(script);
}

// Delay loading MathJax
setTimeout(loadMathJax, 500);

declare class ClipboardItem {
  constructor(data: { [mimeType: string]: Blob });
}

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

function innerInit(Elm: ElmType, saved: { [key: string]: string }) {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  var app = Elm.UI.init({
    node: node,
    flags: {
      saved: saved,
      hasClipboard: typeof ClipboardItem !== "undefined",
      languages: "languages" in navigator ? navigator.languages : [],
      rootUrl: window.location.toString(),
      googleAccessToken: localStorage.getItem("googleAccessToken") ?? "",
    },
  });
  window.addEventListener("storage", (e) => {
    if (e.key == "googleAccessToken")
      app.ports.gotGoogleAccessToken.send(e.newValue ?? "");
  });
  app.ports.persist.subscribe((value) => {
    localForage.setItem(storageKey, value);
  });
  app.ports.save.subscribe((id) => {
    var element = document.getElementById(id) as NuPlot;
    element?.save();
  });
  app.ports.copy.subscribe((id) => {
    var element = document.getElementById(id) as NuPlot;
    element?.copy();
  });
  app.ports.saveGoogleAccessToken.subscribe((token) => {
    localStorage.setItem("googleAccessToken", token);
  });
  app.ports.saveGoogleAccessTokenAndCloseWindow.subscribe((token) => {
    localStorage.setItem("googleAccessToken", token);
    window.close();
  });
  app.ports.openWindow.subscribe((url) => {
    window.open(
      url,
      "popup",
      "menubar=no,status=no,toolbar=no,width=400,height=400"
    );
  });
}

export function init(Elm: ElmType) {
  (localForage.getItem(storageKey) as Promise<{ [key: string]: string }>)
    .then((saved) => innerInit(Elm, saved))
    .catch(() => innerInit(Elm, fromLS()));
}
