import { ElmType } from "../elm/UI.elm";
import { NuPlot } from "./nuplot";
import { KaTeXElement } from "./katex";
import localForage from "localforage";

declare class ClipboardItem {
  constructor(data: { [mimeType: string]: Blob });
}

customElements.define("nu-plot", NuPlot);
customElements.define("ka-tex", KaTeXElement);

const storageKey = "documents";

function fromLS() {
  const saved: { [key: string]: string } = {};
  for (let i = 0; i < localStorage.length; i++) {
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

  const app = Elm.UI.init({
    node: node,
    flags: {
      saved: saved,
      hasClipboard: typeof ClipboardItem !== "undefined",
      hasFullscreen: "onfullscreenchange" in node,
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
    const element = document.getElementById(id) as NuPlot;
    element?.save();
  });
  app.ports.fullscreen.subscribe((id) => {
    const element = document.getElementById(id)?.parentElement;
    element?.requestFullscreen();
  });
  app.ports.exitFullscreen.subscribe((id) => document.exitFullscreen());
  if ("onfullscreenchange" in document)
    document.addEventListener("fullscreenchange", (e) =>
      app.ports.isFullscreen.send(document.fullscreenElement !== null)
    );
  app.ports.copy.subscribe((id) => {
    const element = document.getElementById(id) as NuPlot;
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
